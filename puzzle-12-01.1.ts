import getInput from './getInput';

const run = async () => {
  const input = await getInput(1);

  const vals = input
    .split('\n')
    .filter((x) => x)
    .map((s) => parseInt(s, 10));

  const sum = vals.reduce((acc, n) => acc + n, 0);

  console.log(sum);
};

run();
