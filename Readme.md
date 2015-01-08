Fishbowl
========

An enhanced interactive Shell for Common Lisp  (based on IPython)

```
 Fishbowl: an enhanced interactive Common Lisp Shell
(Version 0.3 - Ipython protocol v.4.1)
--> (C) 2014-2015 Frederic Peschanski (cf. LICENSE)
                                 __________       
                                /         /.      
     .-----------------.       /_________/ |      
    /                 / |      |         | |      
   /+================+\ |      | |====|  | |      
   ||Fishbowl        || |      |         | |      
   ||                || |      | |====|  | |      
   ||* (fact 5)      || |      |         | |      
   ||120             || |      |   ___   | |      
   ||                || |      |  |166|  | |      
   ||                ||/@@@    |   ---   | |      
   \+================+/    @   |_________|./.     
                         @           ..  ....'    
     ..................@      __.'. '  ''         
    /oooooooooooooooo//      ///                  
   /................//      /_/                   
   ------------------                          
```

**Important** : this is alpha version non-officially released software... barely (but still) running. **(Don't) use it at your own risk and peril !**

## Requirements ##

To try Fishbowl you need :

 - a Common lisp implementation (Fishbowl is developed using SBCL, and portability is not at this stage a major requirement)

 - Quicklisp (cf. http://www.quicklisp.org/)

 - IPython (only tested with IPython3 v.2.x.0)

## Quick launch ##

For simple interactions on the console, just type:

    sh run-fishbowl.sh console

```
In [1]: (* 2 21)
Out[1]: 42

In [2]: 
```

## Notebooks ##

The real interest of Fishbowl is its use conjointly
 with the IPython notebook frontend. For a try, type:

    sh run-fishbowl.sh notebook

and ... well ... see for yourself.

----

(Not yet really much to) ... have fun !

