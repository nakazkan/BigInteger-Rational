#pragma once
#include <iostream>
#include <vector>
#include <math.h>
#include <iomanip>

class BigInteger;
class Rational;

BigInteger operator/(int x, const BigInteger &other);

BigInteger operator+(int x, const BigInteger &other);

BigInteger operator-(int x, const BigInteger &other);

BigInteger operator*(int x, const BigInteger &other);

BigInteger operator%(int x, const BigInteger &other);

std::ostream &operator<<(std::ostream &out, const BigInteger &other);

bool operator<(int x, const Rational &other);

bool operator>(int x, const Rational &other);

bool operator>=(int x, const Rational &other);

bool operator<=(int x, const Rational &other);

bool operator==(int x, const Rational &other);

bool operator!=(int x, const Rational &other);

bool operator<(const BigInteger &x, const Rational &other);

bool operator>(const BigInteger &x, const Rational &other);

bool operator>=(const BigInteger &x, const Rational &other);

bool operator<=(const BigInteger &x, const Rational &other);

bool operator==(const BigInteger &x, const Rational &other);

bool operator!=(const BigInteger &x, const Rational &other);

Rational operator+(int x, const Rational &other);

Rational operator-(int x, const Rational &other);

Rational operator*(int x, const Rational &other);

Rational operator/(int x, const Rational &other);

Rational operator+(const BigInteger &x, const Rational &other);

Rational operator-(const BigInteger &x, const Rational &other);

Rational operator*(const BigInteger &x, const Rational &other);

Rational operator/(const BigInteger &x, const Rational &other);

class BigInteger {
 public:

  BigInteger() {
    digits.push_back(0);
  }

  BigInteger(int a) {
    if (a < 0) {
      sign = false;
      a = -a;
    }
    digits.push_back(a % base);
    a /= base;
    while (a) {
      digits.push_back(a % base);
      a /= base;
    }
  }

  BigInteger(const BigInteger &other) : digits(other.digits), sign(other.sign) {}

  ~BigInteger() = default;

  BigInteger &operator=(const BigInteger &other) {
    digits = other.digits;
    sign = other.sign;
    return *this;
  }

  BigInteger operator-() const {
    BigInteger copy = *this;
    copy.sign_flip();
    return copy;
  }

  BigInteger &operator++() {
    return *this += 1;
  }

  BigInteger operator++(int) {
    BigInteger copy = *this;
    *this += 1;
    return copy;
  }

  bool operator==(const BigInteger &other) const {
    if (digits.size() != other.digits.size()) {
      return false;
    }
    for (size_t i = 0; i < digits.size(); ++i) {
      if (digits[i] != other.digits[i]) {
        return false;
      }
    }
    return true;
  }

  bool operator==(int x) const {
    return *this == BigInteger(x);
  }

  bool operator!=(const BigInteger &other) const {
    return !(*this == other);
  }

  bool operator!=(int x) const {
    return *this != BigInteger(x);
  }

  bool operator<(const BigInteger &other) const {
    if (sign != other.sign) {
      return other.sign;
    }
    if (digits.size() != other.digits.size()) {
      return (digits.size() < other.digits.size()) ^ !sign;
    }
    for (size_t i = digits.size() - 1; i + 1 > 0; --i) {
      if (digits[i] < other.digits[i]) {
        return sign;
      }
      if (digits[i] > other.digits[i]) {
        return !sign;
      }
    }
    return false;
  }

  bool operator<(int x) const {
    return *this < BigInteger(x);
  }

  bool operator>(const BigInteger &other) const {
    return other < *this;
  }

  bool operator>(int x) const {
    return *this > BigInteger(x);
  }

  bool operator>=(const BigInteger &other) const {
    return !(*this < other);
  }

  bool operator>=(int x) const {
    return *this >= BigInteger(x);
  }

  bool operator<=(const BigInteger &other) const {
    return !(other < *this);
  }

  bool operator<=(int x) const {
    return *this <= BigInteger(x);
  }
  BigInteger &operator+=(const BigInteger &other) {
    if (sign != other.sign) {
      sign = !sign;
      *this -= other;
      sign = !sign;
      fix_BI();
      return *this;
    } else {
      int carry = 0;
      for (size_t i = 0; i < other.digits.size() || carry; ++i) {
        if (i == digits.size()) {
          digits.push_back(0);
        }
        digits[i] += carry + (i < other.digits.size() ? other.digits[i] : 0);
        carry = digits[i] >= base;
        if (carry) {
          digits[i] -= base;
        }
      }
      fix_BI();
      return *this;
    }
  }


  BigInteger &operator+=(int x) {
    return *this += BigInteger(x);
  }

  BigInteger &operator-=(const BigInteger &other) {
    if (sign == other.sign) {
      BigInteger copy;
      if ((sign && *this < other) || (!sign && *this > other)) {
        copy = other;
        std::swap(copy, *this);
        sign = !sign;
      } else {
        copy = other;
      }
      int carry = 0;
      for (size_t i = 0; i < copy.digits.size() || carry; ++i) {
        digits[i] -= carry + (i < copy.digits.size() ? copy.digits[i] : 0);
        carry = digits[i] < 0;
        if (carry) {
          digits[i] += base;
        }
      }
      while (digits.size() > 1 && digits.back() == 0) {
        digits.pop_back();
      }
      fix_BI();
      return *this;
    } else {
      sign ^= true;
      *this += other;
      sign ^= true;
      return *this;
    }

  }

  BigInteger &operator-=(int x) {
    return *this -= BigInteger(x);
  }

  BigInteger operator+(const BigInteger &other) const {
    BigInteger copy = *this;
    return copy += other;
  }

  BigInteger operator+(int x) const {
    BigInteger copy = *this;
    return copy += x;
  }

  BigInteger operator-(const BigInteger &other) const {
    BigInteger copy = *this;
    return copy -= other;
  }

  BigInteger operator-(int x) const {
    BigInteger copy = *this;
    return copy -= x;
  }

  BigInteger &operator*=(int x) {
    digits.push_back(0);
    if (x < 0) {
      sign = !sign;
      x = -x;
    }
    int add = 0;
    for (size_t i = 0; i < digits.size(); ++i) {
      long long curr = add + 1ll * digits[i] * x;
      digits[i] = curr % base;
      add = curr / base;
    }
    fix_BI();
    return *this;
  }

  BigInteger operator*(int x) const {
    BigInteger copy = *this;
    return (copy *= x);
  }

  BigInteger operator*(const BigInteger &other) const {
    BigInteger copy;
    copy.digits.resize(digits.size() + other.digits.size());
    for (size_t i = 0; i < digits.size(); ++i) {
      int add = 0;
      for (size_t j = 0; j < other.digits.size() || add; ++j) {
        long long curr = copy.digits[i + j] + add;
        if (j < other.digits.size()) {
          curr += 1ll * digits[i] * other.digits[j];
        }
        copy.digits[i + j] = curr % base;
        add = curr / base;
      }
    }
    copy.sign = sign == other.sign;
    copy.fix_BI();
    return copy;
  }

  BigInteger &operator*=(const BigInteger &other) {
    return *this = *this * other;
  }

  BigInteger operator/(const BigInteger &other) const {
    BigInteger mod = *this;
    mod.sign = other.sign;
    BigInteger copy = 0;
    while ((other.sign ? mod >= other : mod <= other)) {
      size_t sub_length = mod.length() - other.length();
      if (sub_length == 0) {
        copy += 1;
        mod -= other;
      } else {
        BigInteger curr = 1;
        curr = curr.add_pow(sub_length - 1);
        BigInteger current_add = other.add_pow(sub_length - 1);
        int mod_curr = (mod.first_not_null() <= other.first_not_null() ? 1 : 10);
        curr *= mod_curr;
        current_add *= mod_curr;
        mod -= current_add;
        copy += curr;
      }
    }
    copy.sign = (sign == other.sign);
    copy.fix_BI();
    return copy;
  }

  BigInteger operator/(int x) const {
    return *this / BigInteger(x);
  }

  BigInteger &operator/=(const BigInteger &other) {
    return *this = *this / other;
  }

  BigInteger &operator/=(int x) {
    return *this /= BigInteger(x);
  }

  BigInteger &operator%=(const BigInteger &other) {
    return *this -= *this / other * other;
  }

  BigInteger operator%(const BigInteger &other) const {
    BigInteger copy = *this;
    copy -= *this / other * other;
    return copy;
  }

  std::string toString() const {
    std::string copy;

    if (!sign) {

      copy.push_back('-');
    }

    copy += tostring_copy(digits.back());
    for (size_t i = digits.size() - 1; i > 0; --i) {
      int cnt_null = 9 - length(digits[i - 1]);
      for (int j = 0; j < cnt_null; ++j) {
        copy.push_back('0');
      }
      copy += tostring_copy(digits[i - 1]);
    }
    return copy;
  }

  friend std::istream &operator>>(std::istream &in, BigInteger &other) {
    std::string s;
    in >> s;
    other.build_str(s);
    return in;
  }

  operator bool() const {
    return (digits.size() != 1 || digits.front() != 0);
  }

  operator int() const {
    if (sign) {
      return digits[0];
    } else {
      return -digits[0];
    }
  }

  void sign_flip() {
    if (digits.size() != 1 || digits.front() != 0) sign = !sign;
  }

  BigInteger add_pow(size_t x) const {
    if (x == 0) {
      return *this;
    }
    if (*this == 0) {
      return *this;
    }
    BigInteger copy;
    copy.sign = sign;
    copy.digits.resize(x / 9);
    for (size_t i = 0; i < digits.size(); ++i) {
      copy.digits.push_back(digits[i]);
    }
    x %= 9;
    size_t mod_pow = 1;
    for (size_t i = 0; i < x; ++i) {
      mod_pow *= 10;
    }
    copy *= mod_pow;
    return copy;
  }

  void build_str(const std::string &s) {
    digits.clear();
    sign = s.front() != '-';
    size_t i = s.size() - 1;

    while (i + 1 > 0 && s[i] != '-') {
      digits.push_back(0);
      size_t pow = 1;

      for (size_t j = 0; j < 9 && i + 1 > 0 && s[i] != '-'; ++j, --i) {
        digits.back() += pow * (s[i] - '0');
        pow *= 10;
      }
    }
  }

  size_t length() const {
    return length(digits.back()) + 9 * (digits.size() - 1);
  }

 private:
  int base = 1000000000;
  std::vector<int> digits;
  bool sign = true;
 private:
  void fix_BI() {
    while (digits.size() > 1 && !digits.back()) {
      digits.pop_back();
    }
    if (digits[0] == 0 && !sign && digits.size() == 1) {
      sign = true;
    }
  }
  size_t length(long long a) const {
    size_t length = 1;
    while (a > 9) {
      a /= 10;
      ++length;
    }
    return length;
  }

  int first_not_null() const {
    int copy = digits.back();
    while (copy > 9) {
      copy /= 10;
    }
    return copy;
  }
  std::string tostring_copy(int x) const {
    std::string copy;
    while (x > 9) {
      copy.push_back(x % 10 + '0');
      x /= 10;
    }
    copy.push_back(x + '0');
    int p1 = 0;
    int p2 = copy.size();
    while (p1 < p2) {
      std::swap(copy[p1++], copy[--p2]);
    }
    return copy;
  }

};
BigInteger operator/(int x, const BigInteger &other) {
  return BigInteger(x) / other;
}

BigInteger operator+(int x, const BigInteger &other) {
  return BigInteger(x) + other;
}

BigInteger operator-(int x, const BigInteger &other) {
  return BigInteger(x) - other;
}

BigInteger operator*(int x, const BigInteger &other) {
  return BigInteger(x) * other;
}

BigInteger operator%(int x, const BigInteger &other) {
  return BigInteger(x) % other;
}
std::ostream &operator<<(std::ostream &out, const BigInteger &other) {

  return out << other.toString();
}
class Rational {
 public:
  Rational(const Rational &other) : p(other.p), q(other.q) {}
  Rational(const BigInteger &x) : p(x), q(1) {}

  Rational(int x) : p(x), q(1) {}

  Rational() = default;

  ~Rational() = default;

  Rational &operator=(const Rational &other) {

    p = other.p;
    q = other.q;
    return *this;
  }

  Rational &operator=(const BigInteger &other) {
    p = other;
    q = 1;
    return *this;
  }

  Rational &operator=(int x) {
    p = BigInteger(x);
    q = 1;
    return *this;
  }

  Rational operator-() const {
    Rational copy = *this;
    copy.p.sign_flip();
    return copy;
  }

  Rational &operator+=(const Rational &other) {

    BigInteger another_P = (other.p * q);
    p *= other.q;
    q *= other.q;
    p += another_P;

    fix_R();
    return *this;
  }

  Rational operator+(const Rational &other) const {
    Rational copy = *this;
    return copy += other;
  }

  Rational &operator-=(const Rational &other) {
    return *this += (-other);
  }

  Rational operator-(const Rational &other) const {
    Rational copy = *this;
    return copy -= other;
  }

  Rational &operator*=(const Rational &other) {
    p *= other.p;
    q *= other.q;

    fix_R();
    return *this;
  }

  Rational operator*(const Rational &other) const {
    Rational copy = *this;
    return copy *= other;
  }

  Rational &operator/=(const Rational &other) {
    p *= other.q;
    q *= other.p;
    if (q < 0) {
      p.sign_flip();
      q.sign_flip();
    }
    fix_R();
    return *this;
  }

  Rational operator/(const Rational &other) const {
    Rational copy = *this;
    return copy /= other;
  }
  bool operator==(const Rational &other) const {
    return (p == other.p) && (q == other.q);
  }
  bool operator!=(const Rational &other) const {
    return !(*this == other);
  }
  bool operator<(const Rational &other) const {
    return (p * other.q < other.p * q);
  }
  bool operator>(const Rational &other) const {
    return other < *this;
  }
  bool operator>=(const Rational &other) const {
    return !(*this < other);
  }
  bool operator<=(const Rational &other) const {
    return !(other < *this);
  }
  Rational &operator+=(const BigInteger &x) {
    return *this += Rational(x);
  }
  Rational operator+(const BigInteger &x) const {
    return *this + Rational(x);
  }
  Rational &operator-=(const BigInteger &x) {
    return *this -= Rational(x);
  }
  Rational operator-(const BigInteger &x) const {
    return *this - Rational(x);
  }
  Rational &operator*=(const BigInteger &x) {
    return *this *= Rational(x);
  }
  Rational operator*(const BigInteger &x) const {
    return *this * Rational(x);
  }
  Rational &operator/=(const BigInteger &x) {
    return *this /= Rational(x);
  }
  Rational operator/(const BigInteger &x) const {
    Rational copy = *this;
    return copy /= x;
  }
  bool operator==(const BigInteger &x) const {
    return *this == Rational(x);
  }
  bool operator!=(const BigInteger &x) const {
    return !(*this == x);
  }
  bool operator<(const BigInteger &x) const {
    return *this < Rational(x);
  }
  bool operator>(const BigInteger &x) const {
    return Rational(x) < *this;
  }
  bool operator<=(const BigInteger &x) const {
    return !(Rational(x) < *this);
  }
  bool operator>=(const BigInteger &x) const {
    return !(*this < Rational(x));
  }
  Rational &operator+=(int x) {
    return *this += Rational(x);
  }
  Rational operator+(int x) const {
    return *this + Rational(x);
  }
  Rational &operator-=(int x) {
    return *this -= Rational(x);
  }
  Rational operator-(int x) const {
    return *this - Rational(x);
  }
  Rational &operator*=(int x) {
    return *this *= Rational(x);
  }
  Rational operator*(int x) const {
    return *this * Rational(x);
  }
  Rational &operator/=(int x) {
    return *this /= Rational(x);
  }
  Rational operator/(int x) const {
    Rational copy = *this;
    return copy /= x;
  }
  bool operator==(int x) const {
    return *this == Rational(x);
  }
  bool operator!=(int x) const {
    return !(*this == x);
  }
  bool operator<(int x) const {
    return *this < Rational(x);
  }
  bool operator>(int x) const {
    return Rational(x) < *this;
  }
  bool operator<=(int x) const {
    return !(Rational(x) < *this);
  }
  bool operator>=(int x) const {
    return !(*this < Rational(x));
  }
  std::string toString() const {
    if (q == 1) {
      return p.toString();
    }

    return p.toString() + '/' + q.toString();
  }

  std::string asDecimal(size_t precision = 0) const {
    std::string copy;

    if (p < 0 && -p <= q) {
      copy.push_back('-');
    }

    copy += (p / q).toString();

    if (precision == 0) return copy;
    copy += '.';
    BigInteger next_to_point = (p.add_pow(precision) / q - (p / q).add_pow(precision));

    if (p < 0) {
      next_to_point.sign_flip();
    }

    int num_null = precision - next_to_point.length();
    for (int i = 0; i < num_null; ++i) {
      copy.push_back('0');
    }
    copy += next_to_point.toString();
    return copy;
  }

  operator double() const {
    return std::stod(asDecimal(18));
  }

 private:
  BigInteger p;
  BigInteger q;

 private:
  void fix_R() {
    BigInteger gcd_ = gcd(p, q);
    p /= gcd_;
    q /= gcd_;
  }

  BigInteger gcd(BigInteger A, BigInteger B) const {
    while (B != 0) {
      BigInteger cnt = A % B;
      A = B;
      B = cnt;
    }
    return (A < 0 ? -A : A);
  }

};

bool operator<(int x, const Rational &other) {
  return Rational(x) < other;
}

bool operator>(int x, const Rational &other) {
  return Rational(x) > other;
}

bool operator>=(int x, const Rational &other) {
  return Rational(x) >= other;
}

bool operator<=(int x, const Rational &other) {
  return Rational(x) <= other;
}

bool operator==(int x, const Rational &other) {
  return Rational(x) == other;
}

bool operator!=(int x, const Rational &other) {
  return Rational(x) != other;
}

bool operator<(const BigInteger &x, const Rational &other) {
  return Rational(x) < other;
}

bool operator>(const BigInteger &x, const Rational &other) {
  return Rational(x) > other;
}

bool operator>=(const BigInteger &x, const Rational &other) {
  return Rational(x) >= other;
}

bool operator<=(const BigInteger &x, const Rational &other) {
  return Rational(x) <= other;
}

bool operator==(const BigInteger &x, const Rational &other) {
  return Rational(x) == other;
}

bool operator!=(const BigInteger &x, const Rational &other) {
  return Rational(x) != other;
}

Rational operator+(int x, const Rational &other) {
  return Rational(x) + other;
}

Rational operator-(int x, const Rational &other) {
  return Rational(x) - other;
}

Rational operator*(int x, const Rational &other) {
  return Rational(x) * other;
}

Rational operator/(int x, const Rational &other) {
  return Rational(x) / other;
}

Rational operator+(const BigInteger &x, const Rational &other) {
  return Rational(x) + other;
}

Rational operator-(const BigInteger &x, const Rational &other) {
  return Rational(x) - other;
}

Rational operator*(const BigInteger &x, const Rational &other) {
  return Rational(x) * other;
}

Rational operator/(const BigInteger &x, const Rational &other) {
  return Rational(x) / other;
}

