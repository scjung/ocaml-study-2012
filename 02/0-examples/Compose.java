abstract class Func {
    abstract public Object eval(Object x);

    // let compose f g x = f (g (x))
    public static Object compose(Func f, Func g, Object x) {
        return f.eval(g.eval(x));
    }
}

public class Compose {
    public static void main(String[] args) {
        Integer n = (Integer)Func.compose(
            // (fun x -> x + 1)
            new Func() {
                public Integer eval(Object x) { return ((Integer)x + 1); }
            },
            // (fun x -> x * 1)
            new Func() {
                public Integer eval(Object x) { return ((Integer)x * 2); }
            },
            5);
        System.out.println(n);
    }
}