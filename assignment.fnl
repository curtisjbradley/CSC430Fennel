;; Structs
(fn numC [n] {:type :numC :val n})
(fn strC [s] {:type :strC :val s})
(fn boolC [c] {:type :boolC :val c})
(fn idC [id] {:type :idC :val id})
(fn condC [c onTrue onFalse] {:type :condC :cond c :onTrue onTrue :onFalse onFalse})
(fn lamC [args body] {:type :lamC :params args :body body})
(fn appC [func args] {:type :appC :func func :args args})

;;Values
(fn numV [n] {:type :numV :val n})
(fn strV [s] {:type :strV :val s})
(fn boolV [v] {:type :boolV :val v})
(fn cloV [params body env] {:type :cloV :params params :body body :env env})
(fn primV [func] {:type :primV :val func})

(fn primsub [args]
  (case args
    [{:type :numV :val a} {:type :numV :val b}] {:type :numV :val (- a b)}
    _ (error "QWJZ Bad input to primsub")))

(fn primadd [args]
  (case args
    [{:type :numV :val a} {:type :numV :val b}] {:type :numV :val (+ a b)}
    _ (error "QWJZ Bad input to primadd")))

(fn primmul [args]
  (case args
    [{:type :numV :val a} {:type :numV :val b}] {:type :numV :val (* a b)}
    _ (error "QWJZ Bad input to primmul")))

(fn primdiv [args]
  (case args
    [{:type :numV :val a} {:type :numV :val b}] {:type :numV :val (/ a b)}
    _ (error "QWJZ Bad input to primdiv")))

(fn primleq [args]
  (case args
    [{:type :numV :val a} {:type :numV :val b}] {:type :boolV :val (<= a b)}
    _ (error "QWJZ Bad input to primleq")))

(fn primeq [args]
  (case args
    [{:type :numV :val a} {:type :numV :val b}] {:type :boolV :val (= a b)}
    _ (error "QWJZ Bad input to primeq")))

(fn primprintln [args]
  (case args
    [{:type _ :val a}] (print a)
    _ (error "Bad input to primprintln"))
  {:type :nullV})

(fn primseq [args]
   (. args (length args)))

(fn primreadint []
  (io.write "> ")
  ; tonumber returns a value or nil, 
  (let [val (tonumber (io.read))]
  (if (not= val nil) {:type :numV :val val} (error "QWJZ: You didn't input a number!")))
  )

(fn primreadstring []
  (io.write "> ")
  {:type :strV :val (io.read)})


(fn strcat [args]
 (accumulate [str "" i val (ipairs args)]
  (case val
   [{:type :strV :val v}] (.. str v)
   [{:type :numV :val n}] (.. str n)
   [{:type :boolV :val false}] (.. str "false")
   [{:type :boolV :val true}] (.. str "true")
   _ (.. str "#<invalid-type>"))))


(fn primerror [args]
  (case args
    [{:type :strV :val a}] (error a)
    _ (error "QWJZ Bad input to primerror")))

(local top-env
  {:+ (primV primadd)
   :- (primV primsub)
   :* (primV primmul)
   :/ (primV primdiv)
   :<= (primV primleq)
   := (primV primeq)
   :println (primV primprintln)
   :seq (primV primseq)
   :++ (primV strcat)
   :error (primV primerror)
   :read-int (primV primreadint)
   :read-str (primV primreadstring)})

(fn extend-env [params args env]
  (each [index param params]
    (tset env param (. args index))))

(fn lookup [id env]
 (let [val (. env id)] (if (= val nil) (error (.. "Value " id " not found in env")) val)))

(fn interp [expr env]
  (match expr
    {:type :numC :val v} (numV v)
    {:type :strC :val s} (strV s)
    {:type :idC :val i} (lookup i env)
    {:type :boolC :val v} (boolV v)
    {:type :condC :cond c :onTrue t :onFalse f}
    (case (interp c env)
      {:type :boolV :val true} (interp t env)
      {:type :boolV :val false} (interp f env)
      _ (error "Conditional condition did not return a bool"))
    {:type :lamC :params p :body b} {:type :cloV :params p :body b :env env}
    {:type :appC :func f :args a}
    (let [clos (interp f env)
          args (icollect [_ v (ipairs a)] (interp v env))]
      (case clos
        {:type :cloV :params p :body b :env e} (interp b (extend-env p args e))
        {:type :primV :val f} (f args)
        _ (error "QWJZ: Invalid closure")))
    _ (error (.. "QWJZ: Bad input. type: " (. expr :type)))))

(fn serialize [val]
  (case val
    {:type :numV :val v} v
    {:type :strV :val s} s
    {:type :boolV :val false} "false"
    {:type :boolv :val true} "true" 
    {:type :nullV} "Null Value"
    nil "Null"
    _ (. val :type)))

(assert (serialize (interp (numC 2))) 2)
(assert (serialize (interp (strC "a"))) "a")
(assert (serialize (interp (appC (idC :+)  [(numC 5) (numC 3)] top-env) top-env)) 8)
(assert (serialize (interp (appC (idC :-)  [(numC 5) (numC 3)] top-env) top-env)) 2)
(assert (serialize (interp (appC (idC :*)  [(numC 5) (numC 3)] top-env) top-env)) 15)
(assert (serialize (interp (appC (idC :/)  [(numC 5) (numC 3)] top-env) top-env)) (/ 5 3))

(assert (serialize (interp (condC (appC (idC :<=) [(appC (idC :+)  [(numC 5) (numC 3)] top-env) (numC 2)])
  (strC "Hello World") (boolC false)) top-env)) "false")

(print (serialize (interp {:type :condC :cond {:type :appC :func {:type :idC :val :<=} 
 :args [{:type :appC :func {:type :idC :val :*} :args [{:type :numC :val 10} {:type :numC :val 3}]} {:type :numC :val 50}]}
 :onTrue {:type :strC :val "Hello World"} :onFalse {:type :boolC :val false}}  top-env)))
(print (serialize (interp {:type :appC :func {:type :idC :val :seq} :args [
  {:type :appC :func {:type :idC :val :println} :args [{:type :strC :val "Printing line 1"}]}
  {:type :appC :func {:type :idC :val :println} :args [{:type :strC :val "Printing line 2"}]}
  {:type :appC :func {:type :idC :val :println} :args [{:type :strC :val "Printing line 3"}]}
  {:type :numC :val 3.14}
 ]} top-env)))
(print (serialize (interp {:type :condC :cond {:type :appC :func {:type :idC :val :<=} 
 :args [{:type :appC :func {:type :idC :val :*} :args [{:type :numC :val 10} {:type :numC :val 3}]} {:type :numC :val 50}]}
 :onTrue {:type :strC :val "Hello World"} :onFalse {:type :boolC :val false}}  top-env)))
(print (serialize (interp {:type :appC :func {:type :idC :val :read-int} :args []} top-env)))
(print (serialize (interp {:type :appC :func {:type :idC :val :read-str} :args []} top-env)))

