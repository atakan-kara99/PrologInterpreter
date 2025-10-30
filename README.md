# Prolog Interpreter in Haskell

This project is a small **Prolog interpreter** written in **Haskell**.
It includes a parser, unification engine, substitution system, and an interactive REPL interface supporting both **DFS** and **BFS** search strategies for SLD-resolution.

---

## ✨ Features

* Parses Prolog programs and goals
* Supports Prolog-style terms, variables, rules, and lists
* Implements **unification**
* Performs **SLD resolution**
* Choice of search strategy:

  * Depth-first search (`dfs`)
  * Breadth-first search (`bfs`)
* REPL with commands for loading files and querying

---

## 📂 Project Structure

| File           | Purpose                                         |
| -------------- | ----------------------------------------------- |
| `Type.hs`      | Core data types (Terms, Rules, Programs, Goals) |
| `Parser.hs`    | Text → AST parser for Prolog programs & queries |
| `Pretty.hs`    | Pretty-printing terms & substitutions           |
| `Uni.hs`       | Unification algorithm                           |
| `Subst.hs`     | Substitution operations                         |
| `Vars.hs`      | Variable extraction & fresh name generation     |
| `Rename.hs`    | Variable renaming for rule reuse                |
| `SLD.hs`       | SLD resolution (DFS & BFS)                      |
| `Interface.hs` | Interactive Prolog REPL (`nippl`)               |

---

## 🛠️ Installation

### Requirements

* [GHC / GHCup](https://www.haskell.org/ghcup/) or [Stack](https://docs.haskellstack.org/)
* Cabal (optional)

### Build with Cabal

```bash
cabal build
```

### Build with Stack

```bash
stack build
```

---

## ▶️ Running the Interpreter

Launch the REPL:

```bash
cabal run
```

or

```bash
stack run
```

You should see:

```
Welcome!
Type :h for help.
?-
```

---

## 🧠 REPL Commands

| Command     | Description              |
| ----------- | ------------------------ |
| `:h`        | Show help                |
| `:l <file>` | Load a Prolog program    |
| `:s dfs`    | Set depth-first search   |
| `:s bfs`    | Set breadth-first search |
| `:q`        | Quit                     |
| `<goal>.`   | Evaluate a goal          |

---

## 🧪 Example

### Prolog Program (`example.pl`)

```prolog
parent(john, mary).
parent(mary, alice).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

### Query in REPL

```
?- :l example.pl
Loaded.
?- ancestor(john, X).
X -> mary;
X -> alice;
No more solutions.
```

### Switch to BFS

```
?- :s bfs
Strategy set to bfs.
```

---

## ✅ Status

This is a **teaching/learning interpreter** — it does not aim to fully replicate SWI-Prolog but provides a clear and minimal foundation for exploring **logic programming** concepts in Haskell.
