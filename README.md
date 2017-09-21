# dbcflatten

Replaces all iframes in a Databricks archive with an html link to the underlying website. Intended for use with [https://github.com/TiloWiklund/pinot](https://github.com/TiloWiklund/pinot) to generate usable markdown from Databricks notebooks.

Using Stack run the command `stack setup && stack build` in the projects directory to build the project. The project can then be run using `stack exec dbcflatten` to get basic usage information. For example, to remove all iframes from the file `input.dbc` and save it as `output.dbc`, run the command `stack exec dbcflatten input.dbc output.dbc`.
