(ns erp12.cbgp-lite.lang.lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground Schemas
;; nil? boolean? int? float? char? string? keyword?

(def environment
  [;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Control Flow
   [:= 'if {:s-vars ['t]
            :body   [:=> [:cat boolean? [:s-var 't] [:s-var 't]] [:s-var 't]]}]
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Higher Order Functions
   [:= 'mapv {:s-vars ['a 'b]
              :body   [:=> [:cat [:=> [:cat [:s-var 'a]] [:s-var 'b]]
                                 [:vector [:s-var 'a]]]
                           [:vector [:s-var 'b]]]}]
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Numeric
   [:= 'int-lt [:=> [:cat int? int?] boolean?]]
   [:= 'int-add [:=> [:cat int? int?] int?]]
   [:= 'int-mult [:=> [:cat int? int?] int?]]
   [:= 'int-inc [:=> [:cat int?] int?]]
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Text
   [:= 'str {:s-vars ['t]
             :body   [:=> [:cat [:s-var 't]] string?]}]
   ])

(def alias->symbol
  '{int-lt <
    int-add +
    int-mult *
    int-inc inc})
