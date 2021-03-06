const execute = rawInstructions => {
  const toNumber = n => (Number.isNaN(Number(n)) ? n : Number(n));

  const instructions = rawInstructions
    .trim()
    .split('\n')
    .map(i => i.split(' '))
    .map(i => [i[0], toNumber(i[1]), toNumber(i[2])]);

  const state = { registers: {}, mulTimes: 0 };

  let instructionPointer = 0;
  let c = 0;

  const getValue = ptr => (typeof ptr === 'number' ? ptr : state.registers[ptr] || 0);

  while (true) {
    if (instructionPointer < 0 || instructionPointer >= instructions.length) {
      break;
    }

    const instruction = instructions[instructionPointer];

    if (instruction[0] === 'set') {
      state.registers[instruction[1]] = getValue(instruction[2]);
    } else if (instruction[0] === 'sub') {
      state.registers[instruction[1]] = getValue(instruction[1]) - getValue(instruction[2]);
    } else if (instruction[0] === 'mul') {
      state.mulTimes++;
      state.registers[instruction[1]] = getValue(instruction[1]) * getValue(instruction[2]);
    } else if (instruction[0] === 'jnz') {
      instructionPointer += getValue(instruction[1]) !== 0 ? getValue(instruction[2]) - 1 : 0;
    }

    instructionPointer++;
  }

  return state;
};

module.exports = execute;
