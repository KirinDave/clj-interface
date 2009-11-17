;;;; ===========================================================================
;;;; clj-interface.clj
;;;; ===========================================================================

(ns com.nubgames.clj-interface
  (:import [java.util Date]
           [com.ericsson.otp.erlang
            OtpErlangAtom
            OtpErlangBinary
            OtpErlangBoolean
            OtpErlangChar
            OtpErlangDouble
            OtpErlangInt
            OtpErlangList
            OtpErlangLong
            OtpErlangObject
            OtpErlangRef
            OtpErlangString
            OtpErlangTuple
            OtpNode]))

(load "clj-interface/erlang")
(load "clj-interface/mnesia")
