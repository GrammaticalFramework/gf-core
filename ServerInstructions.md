# GF server installation

1. First make sure your compiler is installed with a flag server: 

```bash
cd gf-core/src/compiler/           
runghc Setup.hs configure -f servef
runghc Setup.hs build
sudo runghc Setup.hs install
```

1. You can test it now by running: 

```bash
gf -server
```

It will also show the root directory (`ROOT_DIR`)
