function(doc){
    if(doc.type){
	emit(doc.category, doc.sub_category);
    }
}