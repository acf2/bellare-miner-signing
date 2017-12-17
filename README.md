# bm-signing
Bellare-Miner scheme for forward secure signing written in Common Lisp

## Usage
To use it from interpreter just load `load.lisp`
To use if from bash execute `bash launch.sh [OPTIONS]`

### Use cases
```
# Fairly obvious
bash launch.sh --generate-key --private-key=<filename> --public-key=<filename> --time-periods=<number of periods>
bash launch.sh --update-key --private-key=<filename>
bash launch.sh --sign --private-key=<filename> --message=<message file> --signature=<signature file>
bash launch.sh --verify --public-key=<filename> --message=<message file> --signature=<signature file>
```

Number of points defaults to 32. Key size defaults to 2048 bits.

If you want to use custom number of key points or custom key size 
you should add to _each_ of the above commands `--points=<points>` and/or `--bits=<bits>` respectively.

Assumed implementation is SBCL. Was manually tested on SBCL 1.3.11.
