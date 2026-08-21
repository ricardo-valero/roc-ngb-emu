# The SoC's joypad port as pure matrix math: the 2x4 button matrix,
# selected by P1/JOYP's group-select bits, read back active-low
# (0 = pressed). The bus owns the stored select bits in `mem` and the
# per-frame `Button` state; it delegates the register read here.

Joypad :: [].{
	Button : { up : Bool, down : Bool, left : Bool, right : Bool, a : Bool, b : Bool, start : Bool, select : Bool }

	# Neutral input: nothing pressed
	none : {} -> Button
	none = |_| {
		up: Bool.False,
		down: Bool.False,
		left: Bool.False,
		right: Bool.False,
		a: Bool.False,
		b: Bool.False,
		start: Bool.False,
		select: Bool.False,
	}

	# P1/JOYP: stored select bits plus the selected group's buttons,
	# active-low. Both groups selected AND together.
	p1 : Button, U8 -> U8
	p1 = |b, stored| {
		sel = stored.bitwise_and(0x30)
		dpad = if sel.bitwise_and(0x10) == 0x00 {
			nibble(b.right, b.left, b.up, b.down)
		} else {
			0x0F
		}
		actions = if sel.bitwise_and(0x20) == 0x00 {
			nibble(b.a, b.b, b.select, b.start)
		} else {
			0x0F
		}
		U8.bitwise_or(0xC0, sel).bitwise_or(dpad.bitwise_and(actions))
	}

	# bits 0-3, low when pressed
	nibble : Bool, Bool, Bool, Bool -> U8
	nibble = |b0, b1, b2, b3|
		bit(b0, 0x01)
			.bitwise_or(bit(b1, 0x02))
			.bitwise_or(bit(b2, 0x04))
			.bitwise_or(bit(b3, 0x08))

	bit : Bool, U8 -> U8
	bit = |pressed, mask| if pressed {
		0x00
	} else {
		mask
	}
}

# Action group selected (bit 5 low): A is bit 0, active-low
expect Joypad.p1({ ..Joypad.none({}), a: Bool.True }, 0x10).bitwise_and(0x0F) == 0x0E

# D-pad group selected (bit 4 low): down is bit 3, active-low
expect Joypad.p1({ ..Joypad.none({}), down: Bool.True }, 0x20).bitwise_and(0x0F) == 0x07

# Both groups selected AND together; none selected reads 0xF
expect Joypad.p1({ ..Joypad.none({}), a: Bool.True, down: Bool.True }, 0x00).bitwise_and(0x0F) == 0x06
expect Joypad.p1({ ..Joypad.none({}), a: Bool.True, down: Bool.True }, 0x30).bitwise_and(0x0F) == 0x0F

# Upper bits read back as 1 plus the stored select bits
expect Joypad.p1(Joypad.none({}), 0x10) == 0xDF
