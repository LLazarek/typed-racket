#lang typed/racket/base

;; Create a few examples and run abstract interpretation

(require
  require-typed-check
  "structs-adapted.rkt"
)
(require/typed/check "ui.rkt"
  [analyze (-> Exp MonoStore)]
  [format-mono-store (-> MonoStore String)])

;; =============================================================================
(define-type MonoStore (HashTable Var (Setof Exp)))

(define new-label gensym)

(: make-ref (-> Var Exp))
(define (make-ref var)
  (Ref (new-label) var))

(: make-lambda (-> (Listof Var) Exp Exp))
(define (make-lambda formals call)
  (Lam (new-label) formals call))

(: make-call (-> Exp Exp * Exp))
(define (make-call fun . args)
  (Call (new-label) fun args))

(: make-let (-> Symbol Exp Exp Exp))
(define (make-let var exp call)
  (make-call (make-lambda (list var) call) exp))

;; -- main

;; simpler math test, compute: (2 * (1 + 3)) = (1 * 2)
(define mid-test
 (make-call (make-lambda '($f96) (make-call (make-lambda '($e97) (make-call (make-ref '$f96) (make-ref '$e97))) (make-lambda '(p1 $k98) (make-call (make-ref '$k98) (make-lambda '(p2 $k99) (make-call (make-ref '$k99) (make-lambda '(pf $k100) (make-call (make-ref '$k100) (make-lambda '(x $k101) (make-call (make-lambda '($f108) (make-call (make-lambda '($e109) (make-call (make-ref '$f108) (make-ref '$e109) (make-lambda '($f102) (make-call (make-lambda '($f106) (make-call (make-lambda '($e107) (make-call (make-ref '$f106) (make-ref '$e107) (make-lambda '($f104) (make-call (make-lambda '($e105) (make-call (make-ref '$f104) (make-ref '$e105) (make-lambda '($e103) (make-call (make-ref '$f102) (make-ref '$e103) (make-ref '$k101))))) (make-ref 'x))))) (make-ref 'pf))) (make-ref 'p2))))) (make-ref 'pf))) (make-ref 'p1))))))))))) (make-lambda '(plus $k110) (make-call (make-lambda '($f111) (make-call (make-lambda '($e112) (make-call (make-ref '$f111) (make-ref '$e112) (make-ref '$k110))) (make-lambda '(m1 $k113) (make-call (make-ref '$k113) (make-lambda '(m2 $k114) (make-call (make-ref '$k114) (make-lambda '(mf $k115) (make-call (make-lambda '($f116) (make-call (make-lambda '($f118) (make-call (make-lambda '($e119) (make-call (make-ref '$f118) (make-ref '$e119) (make-lambda '($e117) (make-call (make-ref '$f116) (make-ref '$e117) (make-ref '$k115))))) (make-ref 'mf))) (make-ref 'm1))) (make-ref 'm2))))))))) (make-lambda '(mult $k120) (make-call (make-lambda '($f121) (make-call (make-lambda '($e122) (make-call (make-ref '$f121) (make-ref '$e122) (make-ref '$k120))) (make-lambda '(n $k123) (make-call (make-ref '$k123) (make-lambda '(rf $k124) (make-call (make-ref '$k124) (make-lambda '(rx $k125) (make-call (make-lambda '($f132) (make-call (make-lambda '($e133) (make-call (make-ref '$f132) (make-ref '$e133) (make-lambda '($f129) (make-call (make-lambda '($e130) (make-call (make-ref '$f129) (make-ref '$e130) (make-lambda '($f126) (make-call (make-lambda '($e127) (make-call (make-ref '$f126) (make-ref '$e127) (make-ref '$k125))) (make-lambda '(id $k128) (make-call (make-ref '$k128) (make-ref 'id))))))) (make-lambda '(ignored $k131) (make-call (make-ref '$k131) (make-ref 'rx))))))) (make-lambda '(g $k134) (make-call (make-ref '$k134) (make-lambda '(h $k135) (make-call (make-lambda '($f136) (make-call (make-lambda '($f138) (make-call (make-lambda '($e139) (make-call (make-ref '$f138) (make-ref '$e139) (make-lambda '($e137) (make-call (make-ref '$f136) (make-ref '$e137) (make-ref '$k135))))) (make-ref 'rf))) (make-ref 'g))) (make-ref 'h))))))) (make-ref 'n))))))))) (make-lambda '(pred $k140) (make-call (make-lambda '($f141) (make-call (make-lambda '($e142) (make-call (make-ref '$f141) (make-ref '$e142) (make-ref '$k140))) (make-lambda '(s1 $k143) (make-call (make-ref '$k143) (make-lambda '(s2 $k144) (make-call (make-lambda '($f147) (make-call (make-lambda '($e148) (make-call (make-ref '$f147) (make-ref '$e148) (make-lambda '($f145) (make-call (make-lambda '($e146) (make-call (make-ref '$f145) (make-ref '$e146) (make-ref '$k144))) (make-ref 's1))))) (make-ref 'pred))) (make-ref 's2))))))) (make-lambda '(sub $k149) (make-call (make-lambda '($f150) (make-call (make-lambda '($e151) (make-call (make-ref '$f150) (make-ref '$e151) (make-ref '$k149))) (make-lambda '(f0 $k152) (make-call (make-ref '$k152) (make-lambda '(x0 $k153) (make-call (make-ref '$k153) (make-ref 'x0))))))) (make-lambda '(church0 $k154) (make-call (make-lambda '($f155) (make-call (make-lambda '($e156) (make-call (make-ref '$f155) (make-ref '$e156) (make-ref '$k154))) (make-lambda '(f1 $k157) (make-call (make-ref '$k157) (make-lambda '(x1 $k158) (make-call (make-lambda '($f159) (make-call (make-lambda '($e160) (make-call (make-ref '$f159) (make-ref '$e160) (make-ref '$k158))) (make-ref 'x1))) (make-ref 'f1))))))) (make-lambda '(church1 $k161) (make-call (make-lambda '($f162) (make-call (make-lambda '($e163) (make-call (make-ref '$f162) (make-ref '$e163) (make-ref '$k161))) (make-lambda '(f2 $k164) (make-call (make-ref '$k164) (make-lambda '(x2 $k165) (make-call (make-lambda '($f166) (make-call (make-lambda '($f168) (make-call (make-lambda '($e169) (make-call (make-ref '$f168) (make-ref '$e169) (make-lambda '($e167) (make-call (make-ref '$f166) (make-ref '$e167) (make-ref '$k165))))) (make-ref 'x2))) (make-ref 'f2))) (make-ref 'f2))))))) (make-lambda '(church2 $k170) (make-call (make-lambda '($f171) (make-call (make-lambda '($e172) (make-call (make-ref '$f171) (make-ref '$e172) (make-ref '$k170))) (make-lambda '(f3 $k173) (make-call (make-ref '$k173) (make-lambda '(x3 $k174) (make-call (make-lambda '($f175) (make-call (make-lambda '($f177) (make-call (make-lambda '($f179) (make-call (make-lambda '($e180) (make-call (make-ref '$f179) (make-ref '$e180) (make-lambda '($e178) (make-call (make-ref '$f177) (make-ref '$e178) (make-lambda '($e176) (make-call (make-ref '$f175) (make-ref '$e176) (make-ref '$k174))))))) (make-ref 'x3))) (make-ref 'f3))) (make-ref 'f3))) (make-ref 'f3))))))) (make-lambda '(church3 $k181) (make-call (make-lambda '($f182) (make-call (make-lambda '($e183) (make-call (make-ref '$f182) (make-ref '$e183) (make-ref '$k181))) (make-lambda '(ta $k184) (make-call (make-ref '$k184) (make-lambda '(tb $k185) (make-call (make-lambda '($f186) (make-call (make-lambda '($e187) (make-call (make-ref '$f186) (make-ref '$e187) (make-ref '$k185))) (make-lambda '(adummy $k188) (make-call (make-ref '$k188) (make-ref 'adummy))))) (make-ref 'ta))))))) (make-lambda '(true $k189) (make-call (make-lambda '($f190) (make-call (make-lambda '($e191) (make-call (make-ref '$f190) (make-ref '$e191) (make-ref '$k189))) (make-lambda '(fa $k192) (make-call (make-ref '$k192) (make-lambda '(fb $k193) (make-call (make-lambda '($f194) (make-call (make-lambda '($e195) (make-call (make-ref '$f194) (make-ref '$e195) (make-ref '$k193))) (make-lambda '(bdummy $k196) (make-call (make-ref '$k196) (make-ref 'bdummy))))) (make-ref 'fb))))))) (make-lambda '(false $k197) (make-call (make-lambda '($f198) (make-call (make-lambda '($e199) (make-call (make-ref '$f198) (make-ref '$e199) (make-ref '$k197))) (make-lambda '(z $k200) (make-call (make-lambda '($f203) (make-call (make-lambda '($e204) (make-call (make-ref '$f203) (make-ref '$e204) (make-lambda '($f201) (make-call (make-lambda '($e202) (make-call (make-ref '$f201) (make-ref '$e202) (make-ref '$k200))) (make-ref 'true))))) (make-lambda '(zx $k205) (make-call (make-ref '$k205) (make-ref 'false))))) (make-ref 'z))))) (make-lambda '(church0? $k206) (make-call (make-lambda '($f207) (make-call (make-lambda '($e208) (make-call (make-ref '$f207) (make-ref '$e208) (make-ref '$k206))) (make-lambda '(yf $k209) (make-call (make-lambda '($f210) (make-call (make-lambda '($e211) (make-call (make-ref '$f210) (make-ref '$e211) (make-ref '$k209))) (make-lambda '(yx $k212) (make-call (make-lambda '($f213) (make-call (make-lambda '($e214) (make-call (make-ref '$f213) (make-ref '$e214) (make-ref '$k212))) (make-lambda '(yv $k215) (make-call (make-lambda '($f218) (make-call (make-lambda '($e219) (make-call (make-ref '$f218) (make-ref '$e219) (make-lambda '($f216) (make-call (make-lambda '($e217) (make-call (make-ref '$f216) (make-ref '$e217) (make-ref '$k215))) (make-ref 'yv))))) (make-ref 'yx))) (make-ref 'yx))))) (make-ref 'yf))))) (make-lambda '(yg $k220) (make-call (make-lambda '($f221) (make-call (make-lambda '($e222) (make-call (make-ref '$f221) (make-ref '$e222) (make-ref '$k220))) (make-ref 'yg))) (make-ref 'yg))))))) (make-lambda '(Y $k223) (make-call (make-lambda '($f224) (make-call (make-lambda '($f226) (make-call (make-lambda '($e227) (make-call (make-ref '$f226) (make-ref '$e227) (make-lambda '($e225) (make-call (make-ref '$f224) (make-ref '$e225) (make-ref '$k223))))) (make-lambda '(church=? $k228) (make-call (make-ref '$k228) (make-lambda '(e1 $k229) (make-call (make-ref '$k229) (make-lambda '(e2 $k230) (make-call (make-lambda '($f258) (make-call (make-lambda '($e259) (make-call (make-ref '$f258) (make-ref '$e259) (make-lambda '($f253) (make-call (make-lambda '($e254) (make-call (make-ref '$f253) (make-ref '$e254) (make-lambda '($f231) (make-call (make-lambda '($e232) (make-call (make-ref '$f231) (make-ref '$e232) (make-ref '$k230))) (make-lambda '(elsedummy1 $k233) (make-call (make-lambda '($f251) (make-call (make-lambda '($e252) (make-call (make-ref '$f251) (make-ref '$e252) (make-lambda '($f249) (make-call (make-lambda '($e250) (make-call (make-ref '$f249) (make-ref '$e250) (make-lambda '($f234) (make-call (make-lambda '($e235) (make-call (make-ref '$f234) (make-ref '$e235) (make-ref '$k233))) (make-lambda '(elsedummy2 $k236) (make-call (make-lambda '($f243) (make-call (make-lambda '($f247) (make-call (make-lambda '($e248) (make-call (make-ref '$f247) (make-ref '$e248) (make-lambda '($f245) (make-call (make-lambda '($e246) (make-call (make-ref '$f245) (make-ref '$e246) (make-lambda '($e244) (make-call (make-ref '$f243) (make-ref '$e244) (make-lambda '($f237) (make-call (make-lambda '($f241) (make-call (make-lambda '($e242) (make-call (make-ref '$f241) (make-ref '$e242) (make-lambda '($f239) (make-call (make-lambda '($e240) (make-call (make-ref '$f239) (make-ref '$e240) (make-lambda '($e238) (make-call (make-ref '$f237) (make-ref '$e238) (make-ref '$k236))))) (make-ref 'church1))))) (make-ref 'e2))) (make-ref 'sub))))))) (make-ref 'church1))))) (make-ref 'e1))) (make-ref 'sub))) (make-ref 'church=?))))))) (make-ref 'false))))) (make-ref 'e2))) (make-ref 'church0?))))))) (make-lambda '(thendummy $k255) (make-call (make-lambda '($f256) (make-call (make-lambda '($e257) (make-call (make-ref '$f256) (make-ref '$e257) (make-ref '$k255))) (make-ref 'e2))) (make-ref 'church0?))))))) (make-ref 'e1))) (make-ref 'church0?))))))))) (make-ref 'Y))) (make-lambda '(church=? $k260) (make-call (make-lambda '($f267) (make-call (make-lambda '($f275) (make-call (make-lambda '($e276) (make-call (make-ref '$f275) (make-ref '$e276) (make-lambda '($f269) (make-call (make-lambda '($f273) (make-call (make-lambda '($e274) (make-call (make-ref '$f273) (make-ref '$e274) (make-lambda '($f271) (make-call (make-lambda '($e272) (make-call (make-ref '$f271) (make-ref '$e272) (make-lambda '($e270) (make-call (make-ref '$f269) (make-ref '$e270) (make-lambda '($e268) (make-call (make-ref '$f267) (make-ref '$e268) (make-lambda '($f261) (make-call (make-lambda '($f265) (make-call (make-lambda '($e266) (make-call (make-ref '$f265) (make-ref '$e266) (make-lambda '($f263) (make-call (make-lambda '($e264) (make-call (make-ref '$f263) (make-ref '$e264) (make-lambda '($e262) (make-call (make-ref '$f261) (make-ref '$e262) (make-ref '$k260))))) (make-ref 'church2))))) (make-ref 'church1))) (make-ref 'mult))))))))) (make-ref 'church3))))) (make-ref 'church1))) (make-ref 'plus))))) (make-ref 'church2))) (make-ref 'mult))) (make-ref 'church=?)))))))))))))))))))))))))))))

(define standard-example
 (make-let
  'id
  (make-lambda '(x k) (make-call (make-ref 'k) (make-ref 'x)))
  (make-call (make-ref 'id)
   (make-lambda '(z) (make-ref 'z))
   (make-lambda '(a) 
    (make-call (make-ref 'id)
     (make-lambda '(y) (make-ref 'y))
     (make-lambda '(b)
      (make-ref 'b)))))))

(: main (-> Natural Exp Void))
(define (main N e)
  (for ([a-k (in-range N)])
    (analyze e)))

(time (main 2 mid-test))