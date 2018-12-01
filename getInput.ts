import * as request from 'web-request';
import * as fs from 'fs';

const getUrl = (day: number) => `https://adventofcode.com/2018/day/${day}/input`;

export default async (day: number) => {
  const session = fs.readFileSync('.session', 'utf8').trim();

  const { content } = await request.get(getUrl(day), {
    headers: {
      cookie: `session=${session};`
    }
  });

  return content;
};
