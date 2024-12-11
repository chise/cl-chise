# CL-CHISE
[CHISE][https://www.chise.org/] implementation based on Common Lisp.


## Installation

0. Please install
   [CL-Concord][https://gitlab.chise.org/CHISE/cl-concord] and setup
   Redis or Valkey server.

1. Please clone this repository in a ql:*local-project-directories*.

```
* ql:*local-project-directories*
(#P"/home/me/quicklisp/local-projects/")
* (quit)

% cd ~/quicklisp/local-projects/

% git clone https://gitlab.chise.org/CHISE/cl-chise.git
```

2. Register it

```
% sbcl

* (ql:register-local-projects)
NIL
* (quit)
```

3. Load it and setup

```
% sbcl

* (ql:quickload :cl-chise)
(chise:setup)
```

When cl-chise is loaded for the first time, the CHISE character
ontology is installed (so it takes a while to start). From next time
onwards, it will start immediately to use the character ontology
installed in Redis or Valkey database number 3.

If you want to clear the installed character ontology:

```
% redis-cli

> select 3
OK

> flushdb
OK
```


## Usage

```
(princ (chise:decode-char "=ucs" #x5B57))
-> 字
#\U5B57

(chise:decode-char "=daikanwa" 12345)
-> #\U63D1

(chise:get-char-attribute #\U5B57 "->Small-Seal@shuowen")
-> (#.(concord:object :character 1259361))

(chise:char-spec (car (chise:get-char-attribute #\U5B57 "->Small-Seal@shuowen")))
-> ((->subsumptive #.(concord:object :character 1259362)
    #.(concord:object :character 1259363) #.(concord:object :character 1259364))
    (<-Small-Seal@shuowen #\U5B57) (=shuowen-jiguge . 51305)
    (shuowen-radical . 525))

(chise:some-in-character-feature
	(lambda (obj val)
	  (when (eql (nth 1 val)(nth 2 val))
	    (format t "~a (~5,'0X) : ~a~%"
		    obj (chise:char-id obj) val))
	  nil)
	"ideographic-structure")
->
#.(concord:object :character 1052973) (10112D) : (⿲ 幺 幺 幺)
#.(concord:object :character 986659) (F0E23) : (⿰
                                                #.(concord:object :character 986660)
                                                #.(concord:object :character 986660))
巜 (05DDC) : (⿰ 𡿨 𡿨)
𠑲 (20472) : (⿰ 僉 僉)
𢩙 (22A59) : (⿰ 启 启)
戔 (06214) : (⿱ 戈 戈)
:
:
:
𰆬 (301AC) : (⿰ 原 原)
nil

(format t "~a" (chise:ideograph-find-products "兟日"))
->
(𬖂 𡄋 𰖽 𥌳 𨯩 𣎯 㬱 𧮂 𰼀 㦧 濳)

(format t "~a" (chise:ideographic-structure-find-chars
	(cdr (assoc 'chise:ideographic-structure
	     (chise:ids-parse-string "⿰車⿱xx")))))
->
(䡔 𬧺 𨎪 輟 𨌹 輚 𨊿)
```
