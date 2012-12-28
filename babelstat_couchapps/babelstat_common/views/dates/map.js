function(doc){
    if(doc.type){
	emit(doc.date,1);
    }
}