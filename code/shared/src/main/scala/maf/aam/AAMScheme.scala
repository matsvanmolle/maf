package maf.aam

import maf.core.* 
import maf.language.scheme._
import maf.language.scheme.lattices.*
import maf.language.scheme.primitives.*
import maf.modular.scheme.SchemeDomain
import maf.language.sexp

import maf.core.MonadJoin.MonadJoinIterableSyntax
import maf.lattice.interfaces.BoolLattice



abstract class AAMScheme(prg: SchemeExp):
    this: SchemeDomain => 

    // shorthands

    type Val = Value 
    type Exp = SchemeExp
    type Var = Identifier
    type Lit = SchemeValue
    type App = SchemeFuncall
    type Adr = Address
    type Ctx = Context
    type Clo = (SchemeLambdaExp, Environment[Adr])
    type Env = Environment[Adr]
    type Sto = CountingStore[Adr, Val]
    type KSto = BasicStore[KAdr, Set[Frame]]

    // parameters

    type Context
    def t0: Context
    def tnext(app: App, clo: Clo, ctx: Ctx): Context

    // addresses

    case class NAdr(nat: String) extends Address:
        def idn = Identity.none
        def printable = false
    case class VAdr(vrb: Var, ctx: Ctx) extends Address:
        def idn = vrb.idn
        def printable = true 
    case class PAdr(exp: Exp, ctx: Ctx) extends Address:
        def idn = exp.idn
        def printable = true
    case class KAdr(exp: Exp, ctx: Ctx) extends Address:
        def idn = exp.idn
        def printable = false

    def kalloc(e: Exp, ctx: Ctx) = KAdr(e, ctx)
    def alloc(v: Var, ctx: Ctx) = VAdr(v, ctx)
    def alloc(e: Exp, ctx: Ctx) = PAdr(e, ctx)

    enum Frame:
        case IffK(c: Exp, a: Exp, ρ: Env, t: Ctx, aₖ: KAdr)
        case SeqK(r: List[Exp], ρ: Env, t: Ctx, aₖ: KAdr)
        case LetK(l: List[(Var, Exp)], v: Var, a: List[(Var, Val)], b: List[Exp], ρ: Env, t: Ctx, aₖ: KAdr)
        case LtsK(l: List[(Var, Exp)], v: Var, b: List[Exp], ρ: Env, t: Ctx, aₖ: KAdr)
        case LtrK(l: List[(Adr, Exp)], a: Adr, b: List[Exp], ρ: Env, t: Ctx, aₖ: KAdr)
        case FunK(a: App, r: List[Exp], ρ: Env, t: Ctx, aₖ: KAdr)
        case ArgK(a: App, f: Val, v: List[Val], r: List[Exp], ρ: Env, t: Ctx, aₖ: KAdr)

    enum Control:
        case Ev(e: Exp, ρ: Env, t: Ctx)
        case Ap(v: Val)

    case class State(c: Control, σ: Sto, σₖ: KSto, aₖ: KAdr) 
    
    import Control.*
    import Frame.*

    private def step(ς: State): Set[State] = ς match
        case State(Ev(e, ρ, t), σ, σₖ, aₖ) => 
            eval(e, ρ, t, σ, σₖ, aₖ) 
        case State(Ap(v), σ, σₖ, aₖ) if !lattice.isBottom(v) =>
            σₖ(aₖ).flatMap(frm => continue(frm, v, σ, σₖ))
        case _ => // stop on bottom
            Set.empty 

    private def continue(f: Frame, v: Val, σ: Sto, σₖ: KSto): Set[State] = f match
        case IffK(c, a, ρ, t, aₖ) =>
            val ifT = if lattice.isTrue(v)  then Set(State(Ev(c, ρ, t), σ, σₖ, aₖ)) else Set.empty
            val ifF = if lattice.isFalse(v) then Set(State(Ev(a, ρ, t), σ, σₖ, aₖ)) else Set.empty
            ifT ++ ifF
        case SeqK(rest, ρ, t, aₖ) =>
            evalSequence(rest, ρ, t, σ, σₖ, aₖ)
        case LetK(bds, vrb, acc, bdy, ρ, t, aₖ) =>
            evalLet(bds, (vrb, v) :: acc, bdy, ρ, t, σ, σₖ, aₖ)
        case LtsK(bds, vrb, bdy, ρ, t, aₖ) => 
            val adr = alloc(vrb, t)
            evalLetStar(bds, bdy, ρ.extend(vrb.name, adr), t, σ.extend(adr, v), σₖ, aₖ)
        case LtrK(bds, adr, bdy, ρ, t, aₖ) => 
            evalLetrec(bds, bdy, ρ, t, σ.extend(adr, v), σₖ, aₖ)
        case FunK(app, as, ρ, t, aₖ) =>
            evalArgs(app, v, Nil, as, ρ, t, σ, σₖ, aₖ)
        case ArgK(app, f, vs, as, ρ, t, aₖ) =>
            evalArgs(app, f, v :: vs, as, ρ, t, σ, σₖ, aₖ)

    private def evalArgs(app: App, f: Val, vs: List[Val], as: List[Exp], ρ: Env, t: Ctx, σ: Sto, σₖ: KSto, aₖ: KAdr) = as match
        case Nil =>
            apply(app, f, vs.reverse, t, σ, σₖ, aₖ)
        case arg :: rst =>
            push(ArgK(app, f, vs, rst, ρ, t, aₖ), arg, ρ, t, σ, σₖ)

    private def apply(app: App, f: Val, vs: List[Val], t: Ctx, σ: Sto, σₖ: KSto, aₖ: KAdr): Set[State] =
        applyPrimitives(app, f, vs, t, σ, σₖ, aₖ) ++ applyClosures(app, f, vs, t, σ, σₖ, aₖ)

    private def applyPrimitives(app: App, f: Val, vs: List[Val], t: Ctx, σ: Sto, σₖ: KSto, aₖ: KAdr): Set[State] =
        lattice.getPrimitives(f).flatMap { prm =>
            primitives(prm).call[AAM](app, vs)
                           .apply(t, σ)
                           .map((v, σ2) => State(Ap(v), σ2, σₖ, aₖ))
        }

    private def copyAddr(adr: Adr, t: Ctx): Adr = adr match
        case VAdr(vrb, _)   => VAdr(vrb, t)
        case PAdr(exp, _)   => PAdr(exp, t) 
        case KAdr(exp, _)   => KAdr(exp, t)
        case NAdr(nat)      => adr

    private def applyClosures(app: App, f: Val, vs: List[Val], t: Ctx, σ: Sto, σₖ: KSto, aₖ: KAdr): Set[State] =
        val agc = vs.length
        lattice.getClosures(f).collect { case clo@(lam, lex) if lam.check(agc) =>
            // new context on function application
            val ctx = tnext(app, clo, t)
            // collect fixed args bindings
            val fxa = lam.args.zip(vs).map((p, v) => (p.name, alloc(p, ctx), v))
            // collect vararg binding (if present)
            val (vra, σ2) = lam.varArgId.map { varArg => 
                val len = lam.args.length
                val (_, vag) = vs.splitAt(len)
                val (_, vex) = app.args.splitAt(len)
                val (l, σ2) = allocLst(vex.zip(vag), t, σ) 
                (List((varArg.name, alloc(varArg, ctx), l)), σ2)
            }.getOrElse((Nil, σ))
            // collect bindings for free vars
            val fva = lex.toList.map((v, a) => (v, copyAddr(a, ctx), σ(a)))
            // bind everything and eval
            val (ext, σ3) = bind(fxa ++ vra ++ fva, lex, σ2)
            State(Ev(SchemeBody(lam.body), ext, ctx), σ3, σₖ, aₖ)
        }

    private def bind(bds: Iterable[(String, Address, Val)], ρ: Env, σ: Sto): (Env, Sto) =
        bds.foldLeft((ρ, σ)) { case ((accρ, accσ), (n, a, v)) => (accρ.extend(n, a), accσ.extend(a, v)) }

    private def allocLst(els: List[(Exp, Val)], t: Ctx, σ: Sto): (Val, Sto) = els match
        case Nil => 
            (lattice.nil, σ)
        case (exp, car) :: rst => 
            val (cdr, σ2) = allocLst(rst, t, σ)
            val adr = alloc(exp, t)
            val pai = lattice.cons(car, cdr)
            (lattice.pointer(adr), σ2.extend(adr, pai))

    private def eval(e: Exp, ρ: Env, t: Ctx, σ: Sto, σₖ: KSto, aₖ: KAdr): Set[State] = e match
        case l: SchemeValue => 
            evalLiteral(l, σ, σₖ, aₖ, t)
        case λ: SchemeLambdaExp => 
            result(lattice.closure((λ, ρ)), σ, σₖ, aₖ)
        case SchemeVar(v) => 
            result(σ(ρ(v.name)), σ, σₖ, aₖ)
        case SchemeIf(p, c, a, _) => 
            push(IffK(c, a, ρ, t, aₖ), p, ρ, t, σ, σₖ)
        case SchemeBegin(eps, _) => 
            evalSequence(eps, ρ, t, σ, σₖ, aₖ)
        case SchemeAssert(e, _) =>
            evalAssert(e, ρ, t, σ, σₖ, aₖ)
        case SchemeLet(bds, bdy, _) =>
            evalLet(bds, Nil, bdy, ρ, t, σ, σₖ, aₖ)
        case SchemeLetStar(bds, bdy, _) =>
            evalLetStar(bds, bdy, ρ, t, σ, σₖ, aₖ)
        case SchemeLetrec(bds, bdy, _) =>
            val (vrs, eps) = bds.unzip
            val ads = vrs.map(alloc(_, t))
            evalLetrec(ads.zip(eps), bdy, ρ.extend(vrs.map(_.name).zip(ads)), t, σ, σₖ, aₖ) 
        case app@SchemeFuncall(f, as, _) =>
            push(FunK(app, as, ρ, t, aₖ), f, ρ, t, σ, σₖ)
        case _ =>
            throw new Exception(s"Unsupported Scheme expression: $e")
            

    private def evalLiteral(l: Lit, σ: Sto, σₖ: KSto, aₖ: KAdr, t: Ctx) = l.value match
        case sexp.Value.String(s) => 
            val a = alloc(l, t)
            val v = lattice.pointer(a)
            result(v, σ.extend(a, lattice.string(s)), σₖ, aₖ)
        case other => 
            val v = other match 
                case sexp.Value.Integer(n)   => lattice.number(n)
                case sexp.Value.Real(r)      => lattice.real(r)
                case sexp.Value.Boolean(b)   => lattice.bool(b)
                case sexp.Value.Character(c) => lattice.char(c)
                case sexp.Value.Symbol(s)    => lattice.symbol(s)
                case sexp.Value.Nil          => lattice.nil
                case _                       => throw new Exception(s"Unsupported literal: $other")
            result(v, σ, σₖ, aₖ)

    private def evalLet(bds: List[(Var, Exp)], acc: List[(Var, Val)], bdy: List[Exp], ρ: Env, t: Ctx, σ: Sto, σₖ: KSto, aₖ: KAdr) = bds match
        case Nil =>
            val bds = acc.reverse.map { case (vrb, vlu) => (vrb.name, alloc(vrb, t), vlu) } // reverse may not be necessary really (because all identifiers in the same let should be unique)... 
            val (ρ2, σ2) = bind(bds, ρ, σ)
            evalSequence(bdy, ρ2, t, σ2, σₖ, aₖ)
        case (vrb, exp) :: rst =>
            push(LetK(rst, vrb, acc, bdy, ρ, t, aₖ), exp, ρ, t, σ, σₖ)

    private def evalLetStar(bds: List[(Var, Exp)], bdy: List[Exp], ρ: Env, t: Ctx, σ: Sto, σₖ: KSto, aₖ: KAdr) = bds match
        case Nil =>
            evalSequence(bdy, ρ, t, σ, σₖ, aₖ)
        case (vrb, exp) :: rst => 
            push(LtsK(rst, vrb, bdy, ρ, t, aₖ), exp, ρ, t, σ, σₖ) 

    private def evalLetrec(bds: List[(Adr, Exp)], bdy: List[Exp], ρ: Env, t: Ctx, σ: Sto, σₖ: KSto, aₖ: KAdr) = bds match
        case Nil =>
            evalSequence(bdy, ρ, t, σ, σₖ, aₖ)
        case (adr, exp) :: rst =>
            push(LtrK(rst, adr, bdy, ρ, t, aₖ), exp, ρ, t, σ, σₖ)

    private def push(frm: Frame, e: Exp, ρ: Env, t: Ctx, σ: Sto, σₖ: KSto) = 
        val ak2 = kalloc(e, t)
        Set(State(Ev(e, ρ, t), σ, σₖ.extend(ak2, Set(frm)), ak2))

    private def result(v: Val, σ: Sto, σₖ: KSto, aₖ: KAdr) =
        Set(State(Ap(v), σ, σₖ, aₖ))

    private def evalSequence(eps: Iterable[Exp], ρ: Env, t: Ctx, σ: Sto, σₖ: KSto, aₖ: KAdr) = eps match
        case Nil => 
            result(lattice.void, σ, σₖ, aₖ)
        case last :: Nil => 
            Set(State(Ev(last, ρ, t), σ, σₖ, aₖ))
        case next :: rest => 
            push(SeqK(rest, ρ, t, aₖ), next, ρ, t, σ, σₖ)

    // by default, asserts are just ignored
    // this method can be overriden to implement a more interesting analysis w.r.t. assert expressions
    protected def evalAssert(e: Exp, ρ: Env, t: Ctx, σ: Sto, σₖ: KSto, aₖ: KAdr) =
        result(lattice.void, σ, σₖ, aₖ)
    
    // for the primitives

    type AAM[X] = (Ctx, Sto) => Set[(X, Sto)]

    given SchemePrimM[AAM, Address, Val] with
        def lookupSto(a: Address): AAM[Val] = (_, sto) => Set((sto(a), sto))
        def extendSto(a: Address, v: Val) = (_, sto) => Set(((), sto.extend(a, v)))
        def updateSto(a: Address, v: Val) = (_, sto) => Set(((), sto.update(a, v)))
        def allocVar(vrb: Var): AAM[Address] = (ctx, sto) => Set((alloc(vrb, ctx), sto))
        def allocPtr(exp: Exp): AAM[Address] = (ctx, sto) => Set((alloc(exp, ctx), sto))
        def mbottom[X]: AAM[X] = (_, _) => Set.empty
        def mjoin[X: Lattice](x: AAM[X], y: AAM[X]) = (ctx, sto) => x(ctx, sto) ++ y(ctx, sto)
        def unit[X](x: X): AAM[X] = (_, sto) => Set((x, sto))
        def map[X, Y](m: AAM[X])(f: X => Y) = (ctx, sto) => m(ctx, sto).map((a,s) => (f(a), s))
        def flatMap[X, Y](m: AAM[X])(f: X => AAM[Y]) = (ctx, sto) => m(ctx, sto).flatMap((a,s) => f(a)(ctx, s))
        def fail[X](err: Error): AAM[X] = mbottom
        def addrEq: AAM[MaybeEq[Adr]] = (_, sto) => Set((eqA(sto), sto))

    def eqA(sto: Sto): MaybeEq[Adr] = new MaybeEq[Adr]:
        def apply[B: BoolLattice](a1: Adr, a2: Adr): B =
            if a1 == a2 then
                if sto.lookupCount(a1) == CountOne 
                then BoolLattice[B].inject(true)
                else BoolLattice[B].top
            else BoolLattice[B].inject(false)