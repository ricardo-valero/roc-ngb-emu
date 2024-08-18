module [Condition, check]

Condition : [Always, Carry, NotCarry, NotZero, Zero]

import Cpu.Flag

check : Condition, U8 -> Bool
check = \condition, flags ->
    when condition is
        Always -> Bool.true
        Zero -> Cpu.Flag.get Zero flags
        NotZero -> !(Cpu.Flag.get Zero flags)
        Carry -> Cpu.Flag.get Carry flags
        NotCarry -> !(Cpu.Flag.get Carry flags)
