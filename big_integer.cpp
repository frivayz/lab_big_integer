#include "big_integer.h"

BigInteger::BigInteger() {
    digits_.push_back(0);
    negative_ = false;
}




BigInteger::BigInteger(int value) {
    negative_ = value < 0;
    long long abs_value = value; 
    if (abs_value < 0) abs_value = -abs_value;
    
    if (abs_value == 0) {
        digits_.push_back(0);
    } else {
        while (abs_value > 0) {
            digits_.push_back(abs_value % 10);
            abs_value /= 10;
        }
    }
}

BigInteger::BigInteger(long long value) {
    if (value < 0) { negative_ = true; value = -value; }
    else negative_ = false;
    if (value == 0) digits_.push_back(0);
    else while (value > 0) { digits_.push_back(value % 10); value /= 10; }
}

BigInteger::BigInteger(const std::string& str) {
    digits_.clear();
    negative_ = false;
    int start = 0;
    if (!str.empty() && str[0] == '-') {
        negative_ = true;
        start = 1;
    }
    for (int i = str.size() - 1; i >= start; --i)
        digits_.push_back(str[i] - '0');
    while (digits_.size() > 1 && digits_.back() == 0)
        digits_.pop_back();
    if (digits_.size() == 1 && digits_[0] == 0)
        negative_ = false;
}

BigInteger BigInteger::operator+(const BigInteger& rhs) const {
    BigInteger result;
    if (negative_ == rhs.negative_) {
        result.negative_ = negative_;
        result.digits_.clear();
        int carry = 0;
    
        size_t n = digits_.size() > rhs.digits_.size() ? digits_.size() : rhs.digits_.size();
        for (size_t i = 0; i < n; ++i) {
            int sum = carry;
            if (i < digits_.size()) sum += digits_[i];
            if (i < rhs.digits_.size()) sum += rhs.digits_[i];
            result.digits_.push_back(sum % 10);
            carry = sum / 10;
        }
        if (carry) result.digits_.push_back(carry);
    } else {
        if (negative_) {
            BigInteger temp = *this;
            temp.negative_ = false;
            result = rhs - temp;
        } else {
            BigInteger temp = rhs;
            temp.negative_ = false;
            result = *this - temp;
        }
    }
    return result;
}

BigInteger BigInteger::operator-(const BigInteger& rhs) const {
    BigInteger result;
    if (negative_ != rhs.negative_) {
        BigInteger temp = rhs;
        temp.negative_ = !rhs.negative_;
        result = *this + temp;
    } else {
        bool need_swap = false;
        if (digits_.size() < rhs.digits_.size()) need_swap = true;
        else if (digits_.size() == rhs.digits_.size()) {
    
            for (int i = static_cast<int>(digits_.size()) - 1; i >= 0; --i) {
                if (digits_[i] < rhs.digits_[i]) { need_swap = true; break; }
                else if (digits_[i] > rhs.digits_[i]) break;
            }
        }

        const BigInteger* a = this;
        const BigInteger* b = &rhs;
        if (need_swap) { a = &rhs; b = this; }

        result.negative_ = negative_;
        if (need_swap) result.negative_ = !result.negative_;

        result.digits_.clear();
        int borrow = 0; 
       
        for (size_t i = 0; i < a->digits_.size(); ++i) {
            int diff = a->digits_[i] - borrow;
            if (i < b->digits_.size()) diff -= b->digits_[i];
            if (diff < 0) { diff += 10; borrow = 1; }
            else borrow = 0;
            result.digits_.push_back(diff);
        }

        while (result.digits_.size() > 1 && result.digits_.back() == 0)
            result.digits_.pop_back();

        if (result.digits_.size() == 1 && result.digits_[0] == 0)
            result.negative_ = false;
    }
    return result;
}


BigInteger BigInteger::operator*(const BigInteger& rhs) const {
    BigInteger result;
    result.digits_.assign(digits_.size() + rhs.digits_.size(), 0);
    result.negative_ = (negative_ != rhs.negative_);

   
    for (size_t i = 0; i < digits_.size(); ++i) {
        int carry = 0;
      
        for (size_t j = 0; j < rhs.digits_.size() || carry; ++j) {
            int cur = result.digits_[i + j] + 
                      digits_[i] * (j < rhs.digits_.size() ? rhs.digits_[j] : 0) + 
                      carry;
            result.digits_[i + j] = cur % 10;
            carry = cur / 10;
        }
    }

    while (result.digits_.size() > 1 && result.digits_.back() == 0)
        result.digits_.pop_back();

    if (result.digits_.size() == 1 && result.digits_[0] == 0)
        result.negative_ = false;

    return result;
}

BigInteger BigInteger::operator/(const BigInteger& rhs) const {
    BigInteger a = *this;
    BigInteger b = rhs;

    a.negative_ = false;
    b.negative_ = false;

    BigInteger result;
    result.digits_.clear();

    BigInteger cur;
    cur.digits_.push_back(0);

    for (int i = a.digits_.size() - 1; i >= 0; --i) {
        cur.digits_.insert(cur.digits_.begin(), 0);
        cur.digits_[0] = a.digits_[i];

        while (cur.digits_.size() > 1 && cur.digits_.back() == 0)
            cur.digits_.pop_back();

        int x = 0;
        while (!(cur < b)) {
            cur = cur - b;
            x++;
        }

        result.digits_.insert(result.digits_.begin(), x);
    }

    while (result.digits_.size() > 1 && result.digits_.back() == 0)
        result.digits_.pop_back();

    result.negative_ = (negative_ != rhs.negative_);

    if (result.digits_.size() == 1 && result.digits_[0] == 0)
        result.negative_ = false;

    return result;
}

BigInteger BigInteger::operator%(const BigInteger& rhs) const {
    BigInteger res = *this - (*this / rhs) * rhs;
    if (!res.is_zero()) res.negative_ = negative_;
    return res;
}

BigInteger& BigInteger::operator+=(const BigInteger& rhs) { return *this = *this + rhs; }
BigInteger& BigInteger::operator-=(const BigInteger& rhs) { return *this = *this - rhs; }
BigInteger& BigInteger::operator*=(const BigInteger& rhs) { return *this = *this * rhs; }
BigInteger& BigInteger::operator/=(const BigInteger& rhs) { return *this = *this / rhs; }
BigInteger& BigInteger::operator%=(const BigInteger& rhs) { return *this = *this % rhs; }

BigInteger BigInteger::operator-() const {
    BigInteger result = *this;
    if (!result.is_zero()) result.negative_ = !negative_;
    return result;
}

BigInteger& BigInteger::operator++() { return *this += BigInteger(1); }
BigInteger BigInteger::operator++(int) { BigInteger temp = *this; ++(*this); return temp; }
BigInteger& BigInteger::operator--() { return *this -= BigInteger(1); }
BigInteger BigInteger::operator--(int) { BigInteger temp = *this; --(*this); return temp; }

bool BigInteger::operator==(const BigInteger& rhs) const { return digits_ == rhs.digits_ && negative_ == rhs.negative_; }
bool BigInteger::operator!=(const BigInteger& rhs) const { return !(*this == rhs); }

bool BigInteger::operator<(const BigInteger& rhs) const {
    if (negative_ != rhs.negative_)
        return negative_;

    if (!negative_) {
        if (digits_.size() != rhs.digits_.size())
            return digits_.size() < rhs.digits_.size();

        for (int i = digits_.size() - 1; i >= 0; --i)
            if (digits_[i] != rhs.digits_[i])
                return digits_[i] < rhs.digits_[i];
    } else {
        if (digits_.size() != rhs.digits_.size())
            return digits_.size() > rhs.digits_.size();

        for (int i = digits_.size() - 1; i >= 0; --i)
            if (digits_[i] != rhs.digits_[i])
                return digits_[i] > rhs.digits_[i];
    }

    return false;
}

bool BigInteger::operator>(const BigInteger& rhs) const { return rhs < *this; }
bool BigInteger::operator<=(const BigInteger& rhs) const { return !(*this > rhs); }
bool BigInteger::operator>=(const BigInteger& rhs) const { return !(*this < rhs); }

std::string BigInteger::to_string() const {
    std::string s;
    if (negative_) s += '-';
    for (int i = digits_.size() - 1; i >= 0; --i)
        s += char('0' + digits_[i]);
    return s;
}

bool BigInteger::is_zero() const { return digits_.size() == 1 && digits_[0] == 0; }
bool BigInteger::is_negative() const { return negative_; }
BigInteger::operator bool() const { return !is_zero(); }

std::ostream& operator<<(std::ostream& os, const BigInteger& value) {
    os << value.to_string();
    return os;
}

std::istream& operator>>(std::istream& is, BigInteger& value) {
    std::string s;
    is >> s;
    value = BigInteger(s);
    return is;
}