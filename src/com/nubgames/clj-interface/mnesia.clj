;;;; ===========================================================================
;;;; mnesia.clj
;;;; ===========================================================================

(in-ns 'com.nubgames.clj-interface)

(defn mnesia-dirty-all-keys [mbox node tbl]
  (rpc-call mbox node :mnesia :dirty_all_keys tbl))

(defn mnesia-dirty-first [mbox node tbl]
  (rpc-call mbox node :mnesia :dirty_first tbl))

(defn mnesia-dirty-next [mbox node tbl key]
  (rpc-call mbox node :mnesia :dirty_next tbl key))

(defn mnesia-dirty-read [mbox node tbl key]
  (rpc-call mbox node :mnesia :dirty_read tbl key))
