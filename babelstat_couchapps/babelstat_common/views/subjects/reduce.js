function(keys,values,rereduce){
    Array.prototype.unique = function() {    var o = {}, i, l = this.length, r = [];    for(i=0; i<l;i+=1) o[this[i]] = this[i];    for(i in o) r.push(o[i]);    return r;};
    function flatten(array){
	var flat = [];
	for (var i = 0, l = array.length; i < l; i++){
            var type = Object.prototype.toString.call(array[i]).split(' ').pop().split(']').shift().toLowerCase();
            if (type) { flat = flat.concat(/^(array|collection|arguments|object)$/.test(type) ? flatten(array[i]) : array[i]); }
	}
	return flat;
    };
    return flatten(values).unique().sort();
}
