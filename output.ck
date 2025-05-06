

fun void shred_1() {
StepSynth s => Safe safe =>dac;



s.init(
    st.seq([-1.,1])
,
    st.hold(
        st.ch([1.,2,3,4,5,6,7,8,9]),
        st.ch([100.,200,300]))

);




day => now;
}
spork ~ shred_1();

<<<"shred id: ",me.id()>>>;ShredEventStack stack;
ShredEvent end;
stack.push(end);
end => now;