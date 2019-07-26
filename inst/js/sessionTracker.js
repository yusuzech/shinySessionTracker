// start tracking button clicks
// infoLevel controls how much info will be returned to R or console
// available choices are: "all","partial","custom"
shinyjs.trackButtonClick = function(params){

    var defaultParams = {
        eventObserverID : "eventObserver",
        infoLevel : "partial",
        keys : null
    };

    params = shinyjs.getParams(params, defaultParams);

    var eventMeta = {
        elementType:"button",
        eventType:"click"
    };

    $("button").on("click",function(event){
        if(params.infoLevel == "partial"){
            eventType = event.type;
            eleIDclicked = event.target.id;
            eleTagclicked = event.target.localName;

            Shiny.setInputValue(params.eventObserverID,
                {
                    meta:eventMeta,
                    info:{
                        eventType:eventType,
                        eleIDclicked:eleIDclicked,
                        eleTagclicked:eleTagclicked
                }
            });
        } else if(params.infoLevel == "full"){
            Shiny.setInputValue(
                params.eventObserverID,
                {
                    meta:eventMeta,
                    info:event
                }
            );
        } else if(params.infoLevel == "custom"){
        }
    });
};

// Stop tracking button clicks
shinyjs.stoptrackButtonClick = function() {
    $("button").off("click");
};
