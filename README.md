# Dung

An implementation of Dung's argumentation frameworks, an abstract argumentation
model used to either directly represent conflicting information, or used as
a translation target for more complex (structured) argumentation models.

For an introduction to Dung's frameworks see the
[Wikipedia article on argumentation frameworks](http://en.wikipedia.org/wiki/Argumentation_framework)
and Dung's paper from 1995:

> "On the acceptability of arguments and its fundamental role in nonmonotonic
> reasoning, logic programming, and n-person games", Artificial Intelligence
> 77: 321-357.

For the papers accompanying this library see:

- "Towards a framework for the implementation and verification of translations
  between argumentation models"
- "A principled approach to the implementation of argumentation models"

Available at <https://scholar.google.com/citations?user=Xu4yjvwAAAAJ&hl>.

## Usage

```haskell
import Language.Dung

-- Define an argumentation framework: A -> B -> C
let af = AF ["A", "B", "C"] [("A", "B"), ("B", "C")]

-- Compute the grounded extension
groundedExt af
-- ["A", "C"]

-- Compute preferred extensions
preferredExt af
-- [["A","C"]]
```

## Executable

The `dungell` executable reads argumentation frameworks from files in
CEGARTIX/PrefSat format:

```
dungell --filename exampleaf.txt --grounded
dungell --filename exampleaf.txt --preferred
dungell --filename exampleaf.txt --all
```

## License

BSD-3-Clause. See [LICENSE](LICENSE) for details.
