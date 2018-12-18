# Installing HHP and its Emacs front-end

## Installing HHP

```
% cabal update
% cabal install hhp
```

This installs `hhpc` and `hhpi` to "\~/.cabal/bin" and Emacs front-end to "\~/.cabal/share"

## Installing Haskell mode

```
% cd SOMEWHERE
% git clone https://github.com/haskell/haskell-mode.git
% cd haskell-mode
% make
```

## Configuring "\~/.emacs.el"

Add the directories for Emacs front-end and haskell-mode to `load-path`.
Then, put the followings to your "\~/.emacs.el" or "\~/.emacs.d/init.el":

```
(autoload 'hhp-init "hhp" nil t)
(autoload 'hhp-debug "hhp" nil t)
(add-hook 'haskell-mode-hook (lambda () (hhp-init)))
```

IMPORTANT: if your `haskell-mode-hook` includes `(flymake-mode)`, please remove it.

## Testing

Executes Emacs and opens a Haskell file by `C-xC-f`. And try to complete any keywords by `M-C-i`.

## Debugging

`hhpc`/`hhpi` must be compiled by GHC which you are actually using from Emacs. The version of Emacs front-end and `hhpc`/`hhpi` must be the same. To confirm this, type `M-x hhp-debug`.

```
Path: check if you are using intended programs.
	  hhp.el path: /Users/kazu/work/hhp/elisp/hhp.el
	    hhpc path: /Users/kazu/.cabal/bin/hhpc
	    hhpi path: /Users/kazu/.cabal/bin/hhpi
	     ghc path: /usr/local/ghc-8.6.3/bin/ghc

Version: all versions must be the same.
	hhp.el version 0.0.0
	  hhpc version 0.0.0 compiled by GHC 8.6.3
	  hhpi version 0.0.0 compiled by GHC 8.6.3
	The Glorious Glasgow Haskell Compilation System, version 8.6.3
```

If you put `(setq hhp-debug t)` to your ".emacs", you can watch the communication between Emacs front-end and `hhpi` in the "\*HHP Debug\*" buffer.

## Customizing

If you want to specify GHC options from Emacs, set `hhp-ghc-options`. 
The following is an example to specify the `"-i"` options to GHC.

```
(setq hhp-ghc-options '("-idir1" "-idir2"))
```

An example to specify HLint options:

```
(setq hhp-hlint-options '("--ignore=Use camelCase"))
```

