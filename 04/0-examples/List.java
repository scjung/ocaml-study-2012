interface Mapper<V,X> { public X map(V v); }
interface Iterator<V> { public void iter(V v); }
interface Folder<V,X> { public X fold(X x, V v); }

public final class List<V> {
    private final V head;
    private final List<V> tail;

    public List(V h, List<V> t) {
        this.head = h;
        this.tail = t;
    }

    public static <V> List<V> cons(V h, List<V> t) {
        return (new List<V>(h, t));
    }

    public static <V,X> List<X> map(Mapper<V,X> m, List<V> l) {
        if (l == null) {
            return null;
        } else {
            return (new List<X>(m.map(l.head), map(m, l.tail)));
        }
    }

    public static <V> void iter(Iterator<V> i, List<V> l) {
        if (l != null) {
            i.iter(l.head);
            iter(i, l.tail);
        }
    }

    public static <V,X> X fold(Folder<V,X> f, X x, List<V> l) {
        if (l == null) {
            return x;
        } else {
            return fold(f, f.fold(x, l.head), l.tail);
        }
    }

    public static void main(String[] args) {
        List<Integer> l = new List<Integer>(new Integer(1), null);
        l = List.cons(2, l);
        l = List.cons(3, l);
        List.iter(
            new Iterator<Integer>() {
                public void iter(Integer i) { System.out.println(i); }
            }, l);

        List<String> l2 = List.map(
            new Mapper<Integer,String>() {
                public String map(Integer i) {
                    switch (i) {
                    case 1: return "one";
                    case 2: return "two";
                    case 3: return "three";
                    default: return "dunno";
                    }
                }
            }, l);
        List.iter(
            new Iterator<String>() {
                public void iter(String s) { System.out.println(s); }
            }, l2);

        Integer sum = List.fold(
            new Folder<Integer,Integer>() {
                public Integer fold(Integer sum, Integer n) { return sum + n; }
            }, 0, l);
        System.out.println(sum);
    }
}
