# Emanima

WIP https://github.com/EmaApps/ideas/issues/6

Track your mood *delta* over time using merely Markdown notes. Powered by [Emanote](https://github.com/EmaApps/emanote), and extended using [Ema](https://ema.srid.ca).

## Philosophy

Most mood trackers track absolute mood "ratings", but absolute values make little sense because the range of moods is larger than we think. Instead, we track mood "deltas" over time. A "delta" is the difference between the general mood _today_ and the general mood *yesterday*.

The implementation is simple: use daily Markdown notes, and record these delta values in the YAML frontmatter. See ./example.

## Getting Started

This is a Haskell project; see [haskell-template](https://github.com/srid/haskell-template) for general instructions. To run the project locally:

```
bin/run
```