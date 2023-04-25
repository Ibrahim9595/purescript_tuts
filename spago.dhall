{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ "console", "effect", "prelude", "psci-support", "lists", "maybe","tuples","newtype", "strings", "integers","nonempty", "foldable-traversable"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
