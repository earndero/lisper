/*
* Possibilities:
* - only simple params, number args must be equal number params
* - optional are default and not default, not default alhough "optional" are
* obligatory, default args can be from zero to count default optional params.
* Defaults must be after not defaults (are required)
* - keyed are default and not default, if keyed without optional, is similar, except
* than keyed arguments are twice as parameters
* Optional and keyed - all optional parameters myst be default and not default, must
* be all not default Keyed and zero to max default keyed
* */
enum ParamType {NoParam, Simple,OptionalReq, OptionalDef, KeyedReq,KeyedDef}

public class Parameters {
    int simpleCount = 0;
    int optionalReqCount = 0;
    int optionalDefCount = 0;
    int keyedReqCount = 0;
    int keyedDefCount = 0;
    protected Parameters clone() {
        Parameters cloned = new Parameters();
        cloned.simpleCount = simpleCount;
        cloned.optionalReqCount = optionalReqCount;
        cloned.optionalDefCount = optionalDefCount;
        cloned.keyedReqCount = keyedReqCount;
        cloned.keyedDefCount = keyedDefCount;
        return cloned;
    }
}
