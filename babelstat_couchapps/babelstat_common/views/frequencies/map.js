function(doc){
    if(doc.type){
	emit(doc.frequency,1);
    }
}