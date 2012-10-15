abstract class Func {
    abstract public int eval(int x);
}

class Closure {
    public static Func eval(int x) {
        // 클로져로 묶일 값은 모두 final 로 선언되어야 함
        final int dbl = x * 2;

        final Func sq = new Func() {
            public int eval(int x) { return (x * x); }
        };

        return new Func() {
            public int eval(int x) {
                // 여기의 dbl, sq 는 위에서 정의되고 클로져에 묶임
                return dbl + sq.eval(x);
            }
        };
    }

    public static void main(String[] args) {
        Func strange = eval(5);
        System.out.println(strange.eval(3));
    }
}