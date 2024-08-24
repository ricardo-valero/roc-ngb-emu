module [Condition, check]

Condition : [Always, Zero, NotZero, Carry, NotCarry]

import Cpu.Register.Flag

check : Condition, U8 -> Bool
check = \condition, flag ->
    when condition is
        Always -> Bool.true
        Zero -> Cpu.Register.Flag.check Zero flag
        NotZero -> !(Cpu.Register.Flag.check Zero flag)
        Carry -> Cpu.Register.Flag.check Carry flag
        NotCarry -> !(Cpu.Register.Flag.check Carry flag)
