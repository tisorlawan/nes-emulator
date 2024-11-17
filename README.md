Writing NES emulator because ... why not. Not everything has to have a goddamn reason.
Written in Zig.

## Instructions

## 1. Transfers and Loads
### Register Loads
- **LDA** - Load Accumulator
- **LDX** - Load X Register
- **LDY** - Load Y Register

### Register Stores
- **STA** - Store Accumulator
- **STX** - Store X Register
- **STY** - Store Y Register

### Register Transfers
- **TAX** - Transfer Accumulator to X
- **TAY** - Transfer Accumulator to Y
- **TSX** - Transfer Stack Pointer to X
- **TXA** - Transfer X to Accumulator
- **TXS** - Transfer X to Stack Pointer
- **TYA** - Transfer Y to Accumulator

## 2. Stack Operations
### Push Operations
- **PHA** - Push Accumulator
- **PHP** - Push Processor Status

### Pull Operations
- **PLA** - Pull Accumulator
- **PLP** - Pull Processor Status

## 3. Arithmetic Operations
### Addition and Subtraction
- **ADC** - Add with Carry
- **SBC** - Subtract with Carry

### Increment
- **INC** - Increment Memory
- **INX** - Increment X Register
- **INY** - Increment Y Register

### Decrement
- **DEC** - Decrement Memory
- **DEX** - Decrement X Register
- **DEY** - Decrement Y Register

## 4. Logical Operations
- **AND** - Logical AND
- **EOR** - Exclusive OR
- **ORA** - Logical Inclusive OR

## 5. Compare Operations
- **CMP** - Compare Accumulator
- **CPX** - Compare X Register
- **CPY** - Compare Y Register

## 6. Shifts and Rotates
### Shifts
- **ASL** - Arithmetic Shift Left
- **LSR** - Logical Shift Right

### Rotates
- **ROL** - Rotate Left
- **ROR** - Rotate Right

## 7. Jump and Calls
### Jumps
- **JMP** - Jump
- **JSR** - Jump to Subroutine

### Returns
- **RTS** - Return from Subroutine
- **RTI** - Return from Interrupt

## 8. Branch Instructions
### Branch on Flag Clear
- **BCC** - Branch on Carry Clear
- **BVC** - Branch on Overflow Clear

### Branch on Flag Set
- **BCS** - Branch on Carry Set
- **BVS** - Branch on Overflow Set

### Branch on Flag State
- **BEQ** - Branch on Equal (Zero Set)
- **BMI** - Branch on Minus (Negative Set)
- **BNE** - Branch on Not Equal (Zero Clear)
- **BPL** - Branch on Plus (Negative Clear)

## 9. Status Flag Operations
### Clear Flags
- **CLC** - Clear Carry
- **CLD** - Clear Decimal
- **CLI** - Clear Interrupt Disable
- **CLV** - Clear Overflow

### Set Flags
- **SEC** - Set Carry
- **SED** - Set Decimal
- **SEI** - Set Interrupt Disable

## 10. Other Operations
- **BIT** - Bit Test
- **BRK** - Force Break
- **NOP** - No Operation

## Common Addressing Modes
- **Implicit/Implied** - No operand
- **Immediate** - Value provided in instruction
- **Zero Page** - Single byte address (first 256 bytes)
- **Zero Page,X** - Zero page address + X
- **Zero Page,Y** - Zero page address + Y
- **Absolute** - Full 16-bit address
- **Absolute,X** - Absolute address + X
- **Absolute,Y** - Absolute address + Y
- **Indirect** - JMP only
- **Indirect,X** - (Zero page address + X)
- **Indirect,Y** - (Zero page address),Y

## Notes
- Most instructions affect the processor status flags (N, V, Z, C)
- Branch instructions add 1 cycle if branch is taken, 2 if page boundary is crossed
- Some instructions have different cycle counts based on addressing mode
- Page boundary crosses may add additional cycles to some instructions


## References
https://github.com/bugzmanov/nes_ebook/blob/master/code/ch3.3/src/cpu.rs

I use all of these resources below.

- https://bugzmanov.github.io/nes_ebook
  https://github.com/bugzmanov/nes_ebook
- https://www.nesdev.org/obelisk-6502-guide/reference.html
- http://www.6502.org/tutorials/6502opcodes.html
- https://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
