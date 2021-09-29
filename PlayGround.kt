import Fraction.Companion.toFraction
import java.math.BigInteger

class Fraction(numerator: BigInteger, denominator: BigInteger) {

    companion object {

        val ZERO = Fraction(BigInteger.ZERO, BigInteger.ONE)
        val ONE = Fraction(BigInteger.ONE, BigInteger.ONE)

        fun Int.toFraction() = Fraction(BigInteger.valueOf(this.toLong()), BigInteger.ONE)

        private fun calculateGreatestCommonDivider(a: BigInteger, b: BigInteger): BigInteger {
            var (greater, lesser) = maxOf(a, b) to minOf(a, b)
            while (lesser != BigInteger.ZERO) {
                val (newGreater, newLesser) = lesser to (greater % lesser)
                greater = newGreater
                lesser = newLesser
            }
            return greater
        }

    }

    private val numerator: BigInteger
    private val denominator: BigInteger

    init {
        if (denominator == BigInteger.ZERO) throw IllegalArgumentException("Denominator cannot be zero")
        if (numerator == BigInteger.ZERO) {
            this.numerator = BigInteger.ZERO
            this.denominator = BigInteger.ONE
        } else {
            val divider = calculateGreatestCommonDivider(numerator.abs(), denominator.abs())
            val sign = (numerator / numerator.abs()) * (denominator / denominator.abs())
            this.numerator = numerator.abs() / divider * sign
            this.denominator = denominator.abs() / divider
        }
    }

    constructor(numerator: Int, denominator: Int) : this(
        BigInteger.valueOf(numerator.toLong()),
        BigInteger.valueOf(denominator.toLong())
    )

    operator fun unaryPlus() = this

    operator fun unaryMinus() = Fraction(-this.numerator, this.denominator)

    operator fun plus(that: Fraction): Fraction {
        return Fraction(
            this.numerator * that.denominator + this.denominator * that.numerator,
            this.denominator * that.denominator
        )
    }

    operator fun minus(that: Fraction) = this + (-that)

    operator fun times(that: Fraction): Fraction {
        return Fraction(this.numerator * that.numerator, this.denominator * that.denominator)
    }

    operator fun div(that: Fraction) = Fraction(this.numerator * that.denominator, this.denominator * that.numerator)

    override operator fun equals(other: Any?) =
        if (other is Fraction) this.numerator == other.numerator && this.denominator == other.denominator else false

    fun isPositive() = numerator > BigInteger.ZERO

    fun isNegative() = numerator < BigInteger.ZERO

    fun isZero() = numerator == BigInteger.ZERO

    override fun toString(): String {
        return if (numerator == BigInteger.ZERO) "0"
        else if (denominator == BigInteger.ONE) numerator.toString()
        else "\\frac{$numerator}{$denominator}"
    }

    fun abs() = Fraction(numerator.abs(), denominator)
    override fun hashCode(): Int {
        var result = numerator.hashCode()
        result = 31 * result + denominator.hashCode()
        return result
    }
}

class Polynomial(private val terms: List<Fraction>) {
    companion object {
        val X = Polynomial(sequenceOf(0, 1).map { it.toFraction() }.toList())
        val ZERO = Polynomial(Fraction.ZERO)
        val ONE = Polynomial(Fraction.ONE)
    }

    constructor(const: Fraction) :
            this(listOf(const))

    constructor(coefficient: Fraction, power: Int) :
            this(List<Fraction>(power + 1) { index -> if (index == power) coefficient else Fraction.ZERO })

    override fun toString(): String = terms.foldIndexed(StringBuilder()) { power, builder, term ->
        if (term.isZero().not()) {
            if (builder.isNotEmpty())
                builder.append(if (term.isPositive()) " + " else " - ")
            if (builder.isEmpty() && term.isNegative())
                builder.append("-")
            if (term != Fraction.ONE)
                builder.append(term.abs())
            if (term != Fraction.ONE && power > 0)
                builder.append("\\times ")
            if (power > 0)
                builder.append("x")
            if (power > 1)
                builder.append("^{$power}")
        }
        return@foldIndexed builder
    }.toString().takeIf { it.isNotEmpty() } ?: "0"

    operator fun unaryPlus() = this

    operator fun unaryMinus() = Polynomial(this.terms.map { -it })

    operator fun plus(that: Polynomial): Polynomial {
        val maxIndex = maxOf(this.terms.lastIndex, that.terms.lastIndex)
        return Polynomial((0..maxIndex).map { index ->
            this.terms.getOrElse(index) { Fraction.ZERO } + that.terms.getOrElse(
                index
            ) { Fraction.ZERO }
        })
    }

    operator fun minus(that: Polynomial) = this + (-that)

    operator fun plus(that: Int): Polynomial {
        val newList = terms.toMutableList()
        newList[0] += that.toFraction()
        return Polynomial(newList)
    }

    operator fun minus(that: Int): Polynomial = this + (-that)

    operator fun times(that: Polynomial): Polynomial {
        var sum = ZERO

        val thisTerms = this.terms.mapIndexed { index, it -> it to index }
        val thatTerms = that.terms.mapIndexed { index, it -> it to index }

        for ((thisCoefficient, thisPower) in thisTerms)
            for ((thatCoefficient, thatPower) in thatTerms)
                sum += Polynomial(thisCoefficient * thatCoefficient, thisPower + thatPower)

        return sum
    }

    operator fun times(that: Fraction): Polynomial = Polynomial(this.terms.map { it * that })

    fun eval(x: Int): Fraction {
        var powers = 1.toFraction()
        var sum = Fraction.ZERO
        for (coefficient in terms) {
            sum += powers * coefficient
            powers *= x.toFraction()
        }
        return sum
    }
}

fun findPatternForSequence(sequence: List<Int>): Polynomial {
    val x = Polynomial.X
    var sum = Polynomial.ZERO
    sequence.forEachIndexed { index, number ->
        var term = Polynomial.ONE
        val thisXn = index + 1
        val otherXns = (1 until thisXn).asSequence() + ((thisXn + 1)..sequence.count()).asSequence()
        for (otherXn in otherXns) {
            term *= x - otherXn
            term *= Fraction(1, thisXn - otherXn)
        }
        term *= number.toFraction()
        sum += term
    }
    return sum
}

fun main() {
    val f = findPatternForSequence(listOf(1, 2, 4, 8, 16, 30, 114514, 1919810))
    println("f(x) = $f")
    (1..10).forEach { k -> println("f($k) = ${f.eval(k)}") }
}