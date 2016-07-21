# purescript-neo4j

[![Build Status](https://travis-ci.org/corajr/purescript-neo4j.svg?branch=master)](https://travis-ci.org/corajr/purescript-neo4j)

PureScript bindings to the official [Neo4J Javascript Driver](https://github.com/neo4j/neo4j-javascript-driver).

# Example

Assuming you have a type `Person` with `name` and `title` fields, and an appropriate `IsForeign Person` instance:

```purescript
main = do
  results <- withDriver info $ \driver ->
    withSession driver $ \session -> do
      withTransaction session $ \transaction -> do
        execute' (Query "CREATE (a:Person {name:'Arthur', title:'King'})") transaction
        query (Query "MATCH (a:Person) WHERE a.name = {name} RETURN a" :: Query Person) (mkParams {name: "Arthur"}) transaction
  results `shouldEqual` [Person { name: "Arthur", title: "King" }]
```

The
[tests](https://github.com/corajr/purescript-neo4j/blob/master/test/Test/Neo4J.purs)
show additional ways of using the bindings.

# Getting Started

## Installation

```
bower install purescript-neo4j
```

# For Development

## Build

```sh
pulp build
```

## Testing

Install the [Neo4J Community Edition](https://neo4j.com/download/) according to
the instructions for your operating system. The tests assume the username is
"neo4j" and the password is "password4test".

Run `pulp test`.

(Most queries in the tests are made in transactions and immediately rolled back,
but please ensure that you do *not* run the tests against your normal database!)
