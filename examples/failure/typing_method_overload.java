class A extends Object {
    A() {
        super();
    }

    Object method(Object o) {
        return o;
    }
}

class B extends A {
    B() {
        super();
    }

    A method(Object o) {
        return (A)o;
    }

    Object method(A a) {
        return (Object)a;
    }
}
