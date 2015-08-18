class A extends Object {
    Object x;
    A(Object x) {
        super();
        this.x = x;
    }

    Object y(A a) {
        return a.y;
    }
}
