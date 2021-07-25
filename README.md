emacsen
=======

My cool emacs config. Add your own fancy font and modes.

## Packages ##

You'll need these packages.

```elisp
(dolist (x '(buffer-move
             company
             diminish
             doom-themes
             git-gutter
             helm
             magit
             paredit
             powerline
             projectile
             shx
             slime
             smex
             window-numbering
             yasnippet))
  (package-install x))
```
