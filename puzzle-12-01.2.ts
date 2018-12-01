import getInput from './getInput';

const nextValue = (vals: number[]) => (idx: number) => {
  return vals[idx % vals.length];
};

const parseInput = async () => {
  const input = await getInput(1);
  return input
    .split('\n')
    .filter((x) => x)
    .map((s) => parseInt(s, 10));
};

const next = (
  seen: Set<number>,
  nextVal: (idx: number) => number,
  curFreq: number,
  curIdx: number
): number => {
  const cur = nextVal(curIdx);
  const newFreq = curFreq + cur;
  if (seen.has(newFreq)) {
    return newFreq;
  }
  seen.add(newFreq);
  return next(seen, nextVal, newFreq, curIdx + 1);
};

const run = async () => {
  const seen = new Set<number>();

  const values = await parseInput();

  const dupeFreq = next(seen, nextValue(values), 0, 0);

  console.log(dupeFreq);
};

run();
