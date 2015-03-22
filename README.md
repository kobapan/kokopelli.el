kokopelli.el
=============
List up classes and functions.

Jump to the definition.


# Installation
Add kokopelli.el to your load path

Add your .emacs
```cl
(require 'kokopelli)
(define-key global-map [f12] 'kokopelli-sing)
```

# Usage
* Type F12 to list up functions.
* To jump to function's definition, type SPACE or ENTER or DOUBLE LEFT CLICK on any function in the list.
* Type q to quit kokopelli.

# Options
* If you want to close kokopelli window with your selecting a function, add your .emacs
```cl
(setq kokopelli-auto-quit t)
```

* If you want to change the position where the function you selected will appear, add your .emacs
```cl
(setq kokopelli-margin-top 0)
```

0 means the function will appear at the top of the window.

* Window splits vertically by default , when you call kokopelli. If horizontal splits you need, add your .emacs
```cl
(setq kokopelli-split-vertical nil)
```

t : window splits vertically , nil : window splits horizontally
