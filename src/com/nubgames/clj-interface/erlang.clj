;;;; ===========================================================================
;;;; erlang.clj -- Clojure wrapper for Erlang's JInterface.
;;;; ===========================================================================

(in-ns 'com.nubgames.clj-interface)

(defmulti to-erlang   class)
(defmulti from-erlang class)

;; atom
(defmethod to-erlang clojure.lang.Symbol [s]
  (new OtpErlangAtom (name s)))

(defmethod from-erlang OtpErlangAtom [s]
  (symbol (.atomValue s)))

(defmethod to-erlang clojure.lang.Keyword [s]
  (new OtpErlangAtom (name s)))

;; binary -- Can send anything serializable between Java nodes.
(defmethod to-erlang java.lang.Object [o]
  (new OtpErlangBinary o))

(defmethod from-erlang OtpErlangBinary [o]
  (.getObject o))

;; boolean
(defmethod to-erlang java.lang.Boolean [b]
  (new OtpErlangBoolean b))

(defmethod from-erlang OtpErlangBoolean [b]
  (.booleanValue b))

;; char
(defmethod to-erlang java.lang.Character [c]
  (new OtpErlangChar c))

(defmethod from-erlang OtpErlangChar [c]
  (.charValue c))

;; double
(defmethod to-erlang java.lang.Double [d]
  (new OtpErlangDouble d))

(defmethod from-erlang OtpErlangDouble [d]
  (.doubleValue d))

;; int
(defmethod to-erlang java.lang.Integer [i]
  (new OtpErlangInt i))

(defmethod from-erlang OtpErlangInt [i]
  (.intValue i))

;; list
(defmethod to-erlang clojure.lang.Sequential [lst]
  (new OtpErlangList (into-array OtpErlangObject (map to-erlang lst))))

(defmethod from-erlang OtpErlangList [lst]
  (map from-erlang (seq (.elements lst))))

(defmethod to-erlang nil [_]
  (new OtpErlangList (into-array OtpErlangObject [])))

(defmethod from-erlang nil [_]
  nil)

;; long
(defmethod to-erlang java.lang.Long [i]
  (new OtpErlangLong i))

(defmethod from-erlang OtpErlangLong [i]
  (.longValue i))

;; erl -man proplists
(defmethod to-erlang clojure.lang.IPersistentMap [map]
  (to-erlang (seq map)))

;; string
(defmethod to-erlang java.lang.String [s]
  (new OtpErlangString s))

(defmethod from-erlang OtpErlangString [s]
  (.stringValue s))

;; tuple
(defmethod to-erlang clojure.lang.IPersistentVector [vec]
  (new OtpErlangTuple (into-array OtpErlangObject (map to-erlang vec))))

(defmethod from-erlang OtpErlangTuple [vec]
  (apply vector (map from-erlang (seq (.elements vec)))))

;; object
(defmethod to-erlang OtpErlangObject [o]
  o)

(defmethod from-erlang OtpErlangObject [o]
  o)

(def *self* nil)

(defn start-node [name]
  (def *self* (OtpNode. name))
  *self*)

(defn gen-ref []
  (.createRef *self*))

;; Just a wrapper around OtpMbox.send().
(defn erl-send
  ([mbox pid msg]
     (.send mbox pid (to-erlang msg)))
  ([mbox reg node msg]
     (.send mbox (name reg) node (to-erlang msg))))

(defn erl-recv
  ([mbox timeout]
     (from-erlang (.receive mbox timeout)))
  ([mbox]
     (erl-recv mbox 5000)))

;; A cast (one-way notification) using the gen_server protocol.
(defn gen-cast
  ([mbox pid msg]
     (erl-send mbox pid [:$gen_cast msg]))
  ([mbox reg node msg]
     (erl-send mbox reg node [:$gen_cast msg])))

;; A call (two-way request/response) using the gen_server protocol.
(defn gen-call
  ([mbox reg node msg]
     (let [ref  (gen-ref)]
       (erl-send mbox reg node
                 [:$gen_call [(.self mbox) ref] msg])
       (let [[_ result] (erl-recv mbox)]
         result))))

;; rpc:cast(node, module, function, arguments)
(defn rpc-cast [mbox node m f & a]
  (gen-cast mbox :rex node [:cast m f a (.self mbox)]))

;; rpc:call(node, module, function, arguments)
(defn rpc-call [mbox node m f & a]
  (gen-call mbox :rex node [:call m f a (.self mbox)]))

(defn spawn [r]
  (.start (java.lang.Thread. r)))

(defmacro def-record [name slots]
  `(do
     ~@(map (fn [slot i]
              `(defn ~(symbol (str name "-" slot)) [rec#] (nth rec# ~i)))
            slots
            (iterate inc 1))))

(comment
  (rpc-call mbox 'erlang 'echo 'start)
  (erl-send mbox 'echo 'erlang [(.self mbox) "Hello, Erlang!"])
  (erl-recv mbox)
  (rpc-call mbox 'erlang 'echo 'stop)

  (let [mbox (.createMbox self "echo")]
    (spawn
     (fn []
       (let [[pid msg] (erl-recv mbox)]
         (erl-send mbox pid msg)
         (recur)))))
  (erl-send mbox 'echo 'clojure [(.self mbox) "Hello, Clojure!"])
  (erl-recv mbox)

  (rpc-call mbox 'erlang 'proplists 'get_value :a {:a 1 :b 2})

  ;; %% echo.erl
  ;; %% erl -sname erlang
  ;; %% 1> c(echo).
  ;; -module(echo).
  ;; -export([start/0, stop/0]).
  ;; 
  ;; echo() ->
  ;;     receive
  ;;         {Pid, Msg} -> Pid ! Msg, echo();
  ;;         stop       -> ok
  ;;     end.
  ;; 
  ;; start() ->
  ;;     register(echo, spawn(fun() -> echo() end)).
  ;; 
  ;; stop() ->
  ;;     echo ! stop.
)
