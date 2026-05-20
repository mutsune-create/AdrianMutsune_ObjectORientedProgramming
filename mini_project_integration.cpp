#include <iostream>
#include <vector>
#include <cmath>
#include <string>

using namespace std;

class Input {
private:
    double e = 2.71828182845904523536;
public:
    double x;
    string element;
    double numerical_integral = 0;

    Input(double limit = 0, string elem = "") {
        x = limit;
        element = elem;
    }

    double get_e() {
        return e;
    }

    virtual double integrate(string element) = 0;
    virtual ~Input() {}
};

class Spliter {
    string function;
public:
    void spliter(string& equation, vector<string>& V) {
        string element = "";

        for (size_t i = 0; i < equation.length(); i++) {
            if (equation[i] == ' ' || equation[i] == 'y' || equation[i] == '=') {
                equation.erase(i, 1);
                i--;
            }
        }
        function = equation;

        for (size_t j = 0; j < function.length(); j++) {
            if (j != 0 && (function[j] == '+' || function[j] == '-')) {
                V.push_back(element);
                element = function[j];
            } else {
                element += function[j];
            }
        }

        if (!element.empty()) {
            V.push_back(element);
        }
    }
};

class Polynomials : public Input {
public:
    Polynomials(double limit = 0, string elem = "") : Input(limit, elem) {}

    double integrate(string element) override {
        string powers = "";
        double integral_x = 0;
        int position_of_x = -1;
        double coeficient = 1;
        string v = "";

        if (element.length() == 1 && element[0] == 'x') {
            return (x * x) / 2.0;
        }

        for (int i = 0; i < (int)element.length(); i++) {
            if (isdigit(element[i]) || element[i] == '.' ||
                (element[i] == '-' && v.empty())) {
                v += element[i];
            } else if (element[i] == 'x') {
                position_of_x = i;
                if (!v.empty()) {
                    if (v == "-") coeficient = -1;
                    else coeficient = stod(v);
                }
                break;
            }
        }

        if (position_of_x != -1 &&
            position_of_x + 1 < (int)element.length() &&
            element[position_of_x + 1] == '^') {

            for (int i = position_of_x + 2; i < (int)element.length(); i++) {
                if (isdigit(element[i]) || element[i] == '.' ||
                    (element[i] == '-' && powers.empty())) {
                    powers += element[i];
                } else {
                    break;
                }
            }

            double p_val = 1.0;
            if (!powers.empty() && powers != "-") {
                p_val = stod(powers);
            } else if (powers == "-") {
                p_val = -1.0;
            }

            integral_x = coeficient * pow(x, p_val + 1.0) / (p_val + 1.0);

        } else {
            integral_x = coeficient * (x * x) / 2.0;
        }

        return integral_x;
    }
};

class Exponentials : public Input {
public:
    Exponentials(double limit = 0, string elem = "") : Input(limit, elem) {}

    double integrate(string element) override {
        string p = "";
        string c = "";
        double coeficient = 1;
        double powers = 1;
        int position_e = -1;

        // parse coefficient before 'e'
        for (int i = 0; i < (int)element.length(); i++) {
            if (isdigit(element[i]) || element[i] == '.' ||
                (element[i] == '-' && c.empty())) {
                c += element[i];
            } else if (element[i] == 'e') {
                position_e = i;
                break;
            }
        }

        if (!c.empty()) {
            if (c == "-") coeficient = -1;
            else coeficient = stod(c);
        }

        if (position_e != -1 &&
            position_e + 1 < (int)element.length() &&
            element[position_e + 1] == '^' &&
            element.find('x') != string::npos) {

            for (int i = position_e + 2; i < (int)element.length(); i++) {
                if (isdigit(element[i]) || element[i] == '.' ||
                    (element[i] == '-' && p.empty())) {
                    p += element[i];
                }
            }

            if (!p.empty() && p != "-") {
                powers = stod(p);
            } else if (p == "-") {
                powers = -1;
            }

            return (coeficient / powers) * exp(powers * x);

        } else {
            return coeficient * get_e() * x;
        }
    }
};

class Natural_logarithms : public Input {
public:
    Natural_logarithms(double limit = 0, string elem = "") : Input(limit, elem) {}

    double integrate(string element) override {
        string v = "";
        double coeficient = 1.0;
        int position_of_division = -1;

        for (int i = 0; i < (int)element.length(); i++) {
            if (isdigit(element[i]) || element[i] == '.' ||
                (element[i] == '-' && v.empty())) {
                v += element[i];
            } else if (element[i] == '/') {
                position_of_division = i;
                if (!v.empty()) {
                    if (v == "-") coeficient = -1.0;
                    else coeficient = stod(v);
                }
                break;
            }
        }

        if (position_of_division == -1) {
            return 0.0;
        }

        if (x <= 0) return 0.0;
        return coeficient * log(x);
    }
};

class No_x_or_e : public Input {
public:
    No_x_or_e(double limit = 0, string elem = "") : Input(limit, elem) {}

    double integrate(string element) override {
        string coeficient = "";
        for (int i = 0; i < (int)element.length(); i++) {
            if (isdigit(element[i]) || element[i] == '.' ||
                (element[i] == '-' && coeficient.empty())) {
                coeficient += element[i];
            }
        }
        if (coeficient.empty() || coeficient == "-") {
            coeficient = (coeficient == "-") ? "-1" : "1";
        }
        return stod(coeficient) * x;
    }
};

class pure_exponential : public Input {
public:
    pure_exponential(double limit = 0, string elem = "") : Input(limit, elem) {}
    double integrate(string element) override {
        return get_e() * x;
    }
};

void display_UI(double& L, double& U, string& f) {
    cout << "ENTER THE FUNCTION" << endl;
    cin >> f;
    cout << "ENTER THE LOWER LIMIT" << endl;
    cin >> L;
    cout << "ENTER THE UPPER LIMIT" << endl;
    cin >> U;
}

int main() {
    string func;
    double Llimit, Ulimit;

    while (true) {
        vector<string> V;
        display_UI(Llimit, Ulimit, func);
        if (func == "0") break;

        Spliter parser;
        parser.spliter(func, V);

        for (size_t i = 0; i < V.size(); i++) {
            cout << V[i] << endl;
        }

        Exponentials       expObj;
        Natural_logarithms lnObj;
        No_x_or_e          noObj;
        Polynomials        polyObj;
        pure_exponential   pureObj;
        Input* input_ptr = nullptr;

        double Upperlimit_total = 0;
        double Lowerlimit_total = 0;

        for (size_t i = 0; i < V.size(); i++) {

            if (V[i].find('e') != string::npos &&
                V[i].find('^') == string::npos &&
                V[i].find('x') == string::npos) {
                pureObj  = pure_exponential(Ulimit, V[i]);
                input_ptr = &pureObj;
            } else if (V[i].find('e') != string::npos) {
                expObj   = Exponentials(Ulimit, V[i]);
                input_ptr = &expObj;
            } else if (V[i].find('/') != string::npos) {
                lnObj    = Natural_logarithms(Ulimit, V[i]);
                input_ptr = &lnObj;
            } else if (V[i].find('x') != string::npos) {
                polyObj  = Polynomials(Ulimit, V[i]);
                input_ptr = &polyObj;
            } else {
                noObj    = No_x_or_e(Ulimit, V[i]);
                input_ptr = &noObj;
            }
            Upperlimit_total += input_ptr->integrate(V[i]);
        }

        for (size_t i = 0; i < V.size(); i++) {
            if (V[i].find('e') != string::npos &&
                V[i].find('^') == string::npos &&
                V[i].find('x') == string::npos) {
                pureObj  = pure_exponential(Llimit, V[i]);
                input_ptr = &pureObj;
            } else if (V[i].find('e') != string::npos) {
                expObj   = Exponentials(Llimit, V[i]);
                input_ptr = &expObj;
            } else if (V[i].find('/') != string::npos) {
                lnObj    = Natural_logarithms(Llimit, V[i]);
                input_ptr = &lnObj;
            } else if (V[i].find('x') != string::npos) {
                polyObj  = Polynomials(Llimit, V[i]);
                input_ptr = &polyObj;
            } else {
                noObj    = No_x_or_e(Llimit, V[i]);
                input_ptr = &noObj;
            }
            Lowerlimit_total += input_ptr->integrate(V[i]);
        }

        double final_result = Upperlimit_total - Lowerlimit_total;
        cout << "OUTPUT: " << final_result << endl;
    }
    return 0;
}