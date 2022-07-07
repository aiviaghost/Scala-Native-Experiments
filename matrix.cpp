#include <bits/stdc++.h>

using namespace std;

using ll = long long;
using pii = pair<int, int>;
using pll = pair<ll, ll>;
using pdd = pair<double, double>;

template <typename T> vector<T> make(T init, size_t size) { return vector<T>(size, init); }
template <typename T, typename... Args> auto make(T init, size_t first, Args... sizes) {
    auto inner = make<T>(init, sizes...);
    return vector<decltype(inner)>(first, inner);
}

#define all(x) x.begin(), x.end()
#define in(x, a) (x.find(a) != x.end())

const int INF = 2e9;
const int MOD = 1e9 + 7;

#include <ext/pb_ds/assoc_container.hpp>
using namespace __gnu_pbds;
#define unordered_map gp_hash_table

class Matrix {
    private:
        vector<vector<float>> M;
        int N;
    public:
        Matrix(vector<vector<float>> M) : M(M), N(M.size()) {}

        static Matrix* fillRandom(int n) {
            Matrix* res = new Matrix(make<float>(0.0, n, n));
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    res->M[i][j] = static_cast <float> (rand()) / static_cast <float> (RAND_MAX);
                }
            }
            return res;
        }

        Matrix* mult(Matrix* other) {
            Matrix* res = new Matrix(make<float>(0.0, N, N));
            for (int i = 0; i < N; i++) {
                for (int j = 0; j < N; j++) {
                    for (int k = 0; k < N; k++) {
                        res->M[i][j] += M[i][k] * other->M[k][j];
                    }
                }
            }
            return res;
        }
};

auto main() -> int {
    // cin.tie(0)->sync_with_stdio(0);
    cout << fixed << setprecision(10);

    int its = 10;
    int N = 500;
    double tot = 0;
    for (int i = 1; i <= its; i++) {
        cout << "Iteration: " << i << "\n";
        Matrix* A = Matrix::fillRandom(N);
        Matrix* B = Matrix::fillRandom(N);
        auto start = chrono::high_resolution_clock::now();
        Matrix* res = A->mult(B);
        auto finish = chrono::high_resolution_clock::now();
        tot += chrono::duration_cast<std::chrono::nanoseconds>(finish-start).count();
    }
    tot /= its * 1e9;
    cout << tot << "\n";
}
