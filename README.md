# Wireworld
Wireworld is a cellular automaton.

## State of Cell
Cell can be in one of four states below:
- Empty
- Conductor
- Head
- Tail

## Updating Cell
State change follows the rule below:
- Empty --> Empty
- Conductor
  - Head : If the cell has 1 or 2 neighborhood Head cell.
  - Conductor : Otherwise.
- Head --> Tail
- Tail --> Conductor

## Trying Wireworld
I use Elm language to implement Wireworld, and publish to [GitHub Pages](https://water-dropwort.github.io/wireworld/).
