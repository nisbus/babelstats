function(doc){
    if(doc.type){
	emit(doc.metric,1);
    }
}