# MobileDeviceKeyboard
This repository provides a scala interface for suggesting words based on an input prefix.

The application uses an input stream of words to build a vocabulary. Each word is represented as a
path through a Left Child Right Sibling binary tree.

```
Input <- "cat cow dog pig"

    c ------- d ------- p
   /         /         /
  a -- o    o         i
 /    /    /         /
t    w    g         g
```


