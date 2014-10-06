import collections
import sys

def main():
    lines = [[int(x) for x in line.split()] for line in sys.stdin]
    [n, m], a, b, c = lines

    def one():
        return 1

    # http://codereview.stackexchange.com/questions/62956/performance-in-hackerrank-challenge-sherlock-and-queries
    factors = collections.defaultdict(one)
    for i in range(0, m):
        factors[b[i]] = factors[b[i]] * c[i] % 1000000007

    for i, factor in factors.iteritems():
        for idx in xrange(i-1, n, i):
            a[idx] = a[idx] * factor % 1000000007

    print ' '.join(map(str, a))

if __name__ == "__main__":
    main()