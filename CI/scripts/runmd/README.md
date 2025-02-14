# `Runmd` command

The `runmd` command is a simple script that takes 2 arguments and produces a
stream of `bash`-syntax lines on stdout.

First argument is a yaml file which represents a recipe of what to collect.

Second argument is the base directory to prepend to the relative paths in the
recipe.

The recipe is a list of dictionaries, each dictionary has a `path` key and a
`matches` key. The `path` key is a relative path to a markdown file. The
`matches` key is a list of allowances for the bash code blocks in the markdown.

## Recipe example

```yaml
- path: a.md
    matches:
        - A
        - B
- path: b.md
    matches:
        - C
        - D
```
The above recipe will collect the bash code blocks that match the `A` and `B`
labels from the `a.md` file and the bash code blocks that match the `C` and `D`
labels from the `b.md` file.

## Execution order
Each dictionary is executed in the order it appears in the recipe. The `path` is
prepended with the base directory and the markdown file is read. For every
`match` in the dictionary, all code blocks that have one key matching are
streamed out. If a match appears multiple times in the markdown file, the code
blocks matching are all streamed out.

## Multiple keys code blocks

`bash` code blocks with multiple keys will be executed if any of the keys match
the current `matches` key.

## Dictionary multiplicity

Dictionaries with the same `path` can appear multiple times
in the recipe.

```yaml
- path: a.md
    matches:
        - A
        - B
- path: b.md
    matches:
        - C
        - D
- path: a.md
    matches:
        - C
        - D
```

The above is a perfectly legal recipe that will collect the bash code blocks
from `a.md` twice, once for the `A` and `B` labels and once for the `C` and `D`
labels interspersed with the bash code blocks from `b.md` for the `C` and `D`
labels.

## Markdown syntax

Codeblocks are expected to be in the following format:

````markdown
```bash key
echo "code block will be picked up if the label matches the current match"
```
````

The info string is parsed as a CSV line separated by commas so spaces are
allowed. Because there can be multiple keys in the info string, the code
block is matched if any of the keys match.

````markdown
```bash key 1, key 2, key 3 ...
echo "code block will be picked up if any of the labels matches the current match"
```
````
## Usage

### Example

Create a recipe file `recipe.yaml`:

```bash recipe.yaml file
cat << EOF > recipe.yaml

- path: a.md
  matches:
   - A
   - B
- path: b.md
  matches:
   - D # request the code block with label D before the code block with label C
   - C

EOF
```

Create a directory `there`:

```bash there directory
mkdir -p there
```

Create the file `a.md` containing code blocks with the labels `A` and `B`:

````bash a.md file
cat <<'EOF' > there/a.md

Some md text

```bash A
echo "code block A"
```
other md text

```bash B
echo "code block B"
```

EOF
````

Create the file `b.md` containing code blocks with the labels `C` and `D`:

````bash b.md file
cat << 'EOF' > there/b.md

Some md text

```bash C
echo "code block C"
```
other md text

```bash D
echo "code block D"
```

EOF

````

Running the following command:

```bash run
runmd -r recipe.yaml -d there | bash > result.txt
```

The following will hold:

```bash test
cat << EOF > expected.txt
code block A
code block B
code block D
code block C
EOF

diff -u expected.txt result.txt
rm expected.txt result.txt
```

Notice that the `bash` code blocks are printed in the order they appear in the
recipe. So `D` is printed before `C` because it appears before in the recipe.

You can add some logs in between the code blocks to see the order of execution
with the `-l` flag:

```bash run logging
runmd -l -r recipe.yaml -d there | bash > result.txt
```

Then the following will hold:

```bash test logging
cat << EOF > expected.txt
Running a.md A
code block A
Running a.md B
code block B
Running b.md D
code block D
Running b.md C
code block C
EOF

diff -u expected.txt result.txt
rm expected.txt result.txt
```

## Script echoing

You can add line leval echoing to the script by using the `-e` flag:

```bash run echoing
runmd -e -r recipe.yaml -d there | bash 1> result.txt 2>&1
```

Then the following will hold:

```bash test echoing
cat << EOF > expected.txt
+ echo 'code block A'
code block A
+ echo 'code block B'
code block B
+ echo 'code block D'
code block D
+ echo 'code block C'
code block C
EOF

diff -u expected.txt result.txt
rm expected.txt result.txt
```



### Flake

Because the `runmd` command is defined as a nix flake, it can be used directly
in this repository with the following command:

```bash
nix run .CI/scripts/runmd#runmd -r recipe.yaml -d directory | bash
```

Remotely with the following command:

```bash
nix run "github:cardano-foundation/cardano-deposit-wallet?dir=CI/scripts/runmd" -r recipe.yaml -d /path/to/base/directory | bash
```