# Data

The analysis panel is not distributed here. It comes from two sources with
their own terms of use:

| Source | Provides |
|---|---|
| [FanGraphs](https://www.fangraphs.com/) | PA, wOBA, xwOBA, wRC+, BsR, Off, Def, WAR |
| [Cot's Baseball Contracts](http://legacy.baseballprospectus.com/compensation/cots/) | service time, free agency and arbitration eligibility, option years |

The two were hand-merged at the player-season level.

## Rebuilding Clean_Data.csv

Put the merged file at `data/Clean_Data.csv`. The build expects these twenty
columns in this order:

```
PlayerId, MLBAMID, Team, Season, Name, PA, wOBA, xwOBA, wRC+, BsR,
Off, Def, WAR, MLS, FA Elig, Arb Elig, Club, Player, Mutual, Vesting
```

`R/01_build.R` stops with a column diff if the header does not match.

### Sample

All MLB batters 2015-2025, one row per player-season with at least 200 plate
appearances, pitchers excluded. A correct rebuild gives:

- 3,654 player-seasons
- 1,003 players
- 1,692 contract years, of which 528 free agency, 962 arbitration, 202 options

### Service time

`MLS` is in decimal years, not years-and-days. A service year is 172 days, so
a raw `5.155` means five years and 155 days and converts to 5 + 155/172 =
5.901.

`01_build.R` detects which coding it was given by checking whether any
fractional part exceeds .171, which is impossible under years-and-days, and
converts only if needed. Double-converting raises no error, which is why this
is detected rather than assumed.

### One correction

Cervelli's 2018 season circulates with a dropped leading digit: `0.622093`
where the adjacent seasons imply `7.622093`. Service time cannot decrease, so
this is unambiguous. The build repairs it and reports which extract it found,
so either version gives identical output.

It is not a cosmetic fix. It moves the within-player service time coefficient
from -0.0031 to -0.0040, wRC+ from 0.3796 to 0.3074, WAR from 0.1045 to
0.0943, and shifts one season across the five-year veteran threshold. The
contract-year row beside it barely moves, which is what makes a partial
regeneration hard to catch by eye.

### Encoding

The source CSV is CP850, not UTF-8 or Latin-1. The only high bytes are
`A0 A1 A2 A3 A4 82`, which decode under CP850 to the Spanish accents in player
names and under Latin-1 to punctuation, so the encoding is detected rather
than assumed. `read_prelim()` reads raw bytes and decodes explicitly, because
R >= 4.2 on Windows is natively UTF-8 and will mangle CP850 on input.

## Checking your rebuild

Run the pipeline. `01_build.R` gates the data against a fingerprint of means
and counts before any model is estimated, and `04_verify.R` gates 56 estimates
afterward. If the file is right, both pass; if not, the run stops and names
the quantity that disagrees.
