# Test-ROM verdict protocols as pure views of the machine state, shared by
# the check slices: Blargg's serial-text protocol ("Passed"/"Failed"),
# Blargg's memory-reporting protocol (signature DE B0 61 at 0xA001, status
# at 0xA000: 0x80 while running, 0 = pass), and mooneye's serial bytes
# (Fibonacci 3,5,8,13,21,34 = pass, six 0x42 = fail).
import ngb.GameBoy

Harness := [].{
    # Combined verdict across all three protocols
    verdict : GameBoy -> [Running, Passed, Failed]
    verdict = |gb| {
        text = serial_text(gb)
        if text.contains("Passed") {
            Passed
        } else if text.contains("Failed") {
            Failed
        } else if memory_status(gb) == 0 {
            Passed
        } else if memory_status(gb) < 0x80 {
            Failed
        } else if mooneye_status(gb) == 1 {
            Passed
        } else if mooneye_status(gb) == 2 {
            Failed
        } else {
            Running
        }
    }

    serial_text : GameBoy -> Str
    serial_text = |gb| Str.from_utf8(gb.serial()) ?? ""

    has_signature : GameBoy -> Bool
    has_signature = |gb|
        gb.peek(0xA001) == 0xDE and gb.peek(0xA002) == 0xB0 and gb.peek(0xA003) == 0x61

    memory_status : GameBoy -> U8
    memory_status = |gb| if has_signature(gb) { gb.peek(0xA000) } else { 0x80 }

    # Result text of the memory protocol, from 0xA004 (empty without signature)
    memory_text : GameBoy -> Str
    memory_text = |gb| {
        var bytes = [].append(0x20)
        var addr = 0xA004.U16
        var going = has_signature(gb)
        while going and addr < 0xA200 {
            byte = gb.peek(addr)
            if byte == 0x00 {
                going = Bool.False
            } else {
                bytes = bytes.append(byte)
                addr = addr.plus(1)
            }
        }
        Str.from_utf8(bytes) ?? ""
    }

    # 0 running, 1 passed, 2 failed
    mooneye_status : GameBoy -> U8
    mooneye_status = |gb| {
        bytes = gb.serial()
        if bytes.len() >= 6 {
            if serial_prefix_is(bytes, [3, 5, 8, 13, 21, 34]) {
                1
            } else if serial_prefix_is(bytes, [0x42, 0x42, 0x42, 0x42, 0x42, 0x42]) {
                2
            } else {
                0
            }
        } else {
            0
        }
    }

    serial_prefix_is : List(U8), List(U8) -> Bool
    serial_prefix_is = |bytes, want| {
        var ok = Bool.True
        var i = 0
        while i < want.len() {
            if (bytes.get(i) ?? 0xFF) != (want.get(i) ?? 0x00) {
                ok = Bool.False
            } else {
                {}
            }
            i = i.plus(1)
        }
        ok
    }
}

expect Harness.serial_prefix_is([3, 5, 8, 13, 21, 34], [3, 5, 8, 13, 21, 34])
expect Harness.serial_prefix_is([3, 5, 8, 13, 21, 34, 99], [3, 5, 8, 13, 21, 34])
expect Harness.serial_prefix_is([3, 5, 8, 13, 21, 35], [3, 5, 8, 13, 21, 34]) == Bool.False
expect Harness.serial_prefix_is([3, 5], [3, 5, 8, 13, 21, 34]) == Bool.False

# A fresh machine with a NOP-filled ROM reports no verdict
expect {
    gb = GameBoy.init(List.repeat(0x00, 0x8000))
    Harness.verdict(gb) == Running and Harness.memory_status(gb) == 0x80
}
