// reads workout data from workouts.json
// generates 3 graphs for one workout object
// mph graph, mi graph, mets graph
// text animates as graph scales from 0-> length

var columnObjects = 16;

Array.range= function(a, len, step){
    var A= [];
    if(typeof a== 'number'){
        A[0]= a;
        step= step || 1;
        while(A.length<= len){
            A[A.length]= a+= step;
        }
    }
    return A;
}


var getKeysWithoutObjectKeysSupport = function(associativeArrayObject) {
    var arrayWithKeys=[], associativeArrayObject;
    for (key in associativeArrayObject) {
      // Avoid returning these keys from the Associative Array that are stored in it for some reason
      if (key !== undefined && key !== "toJSONString" && key !== "parseJSON" ) {
        arrayWithKeys.push(key);
      }
    }
    return arrayWithKeys;
  }


"object"!=typeof JSON&&(JSON={}),function(){"use strict";var rx_one=/^[\],:{}\s]*$/,rx_two=/\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g,rx_three=/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g,rx_four=/(?:^|:|,)(?:\s*\[)+/g,rx_escapable=/[\\"\u0000-\u001f\u007f-\u009f\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,rx_dangerous=/[\u0000\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,gap,indent,meta,rep;function f(t){return t<10?"0"+t:t}function this_value(){return this.valueOf()}function quote(t){return rx_escapable.lastIndex=0,rx_escapable.test(t)?'"'+t.replace(rx_escapable,function(t){var e=meta[t];return"string"==typeof e?e:"\\u"+("0000"+t.charCodeAt(0).toString(16)).slice(-4)})+'"':'"'+t+'"'}function str(t,e){var r,n,o,u,f,a=gap,i=e[t];switch(i&&"object"==typeof i&&"function"==typeof i.toJSON&&(i=i.toJSON(t)),"function"==typeof rep&&(i=rep.call(e,t,i)),typeof i){case"string":return quote(i);case"number":return isFinite(i)?String(i):"null";case"boolean":case"null":return String(i);case"object":if(!i)return"null";if(gap+=indent,f=[],"[object Array]"===Object.prototype.toString.apply(i)){for(u=i.length,r=0;r<u;r+=1)f[r]=str(r,i)||"null";return o=0===f.length?"[]":gap?"[\n"+gap+f.join(",\n"+gap)+"\n"+a+"]":"["+f.join(",")+"]",gap=a,o}if(rep&&"object"==typeof rep)for(u=rep.length,r=0;r<u;r+=1)"string"==typeof rep[r]&&(o=str(n=rep[r],i))&&f.push(quote(n)+(gap?": ":":")+o);else for(n in i)Object.prototype.hasOwnProperty.call(i,n)&&(o=str(n,i))&&f.push(quote(n)+(gap?": ":":")+o);return o=0===f.length?"{}":gap?"{\n"+gap+f.join(",\n"+gap)+"\n"+a+"}":"{"+f.join(",")+"}",gap=a,o}}"function"!=typeof Date.prototype.toJSON&&(Date.prototype.toJSON=function(){return isFinite(this.valueOf())?this.getUTCFullYear()+"-"+f(this.getUTCMonth()+1)+"-"+f(this.getUTCDate())+"T"+f(this.getUTCHours())+":"+f(this.getUTCMinutes())+":"+f(this.getUTCSeconds())+"Z":null},Boolean.prototype.toJSON=this_value,Number.prototype.toJSON=this_value,String.prototype.toJSON=this_value),"function"!=typeof JSON.stringify&&(meta={"\b":"\\b","\t":"\\t","\n":"\\n","\f":"\\f","\r":"\\r",'"':'\\"',"\\":"\\\\"},JSON.stringify=function(t,e,r){var n;if(gap="",indent="","number"==typeof r)for(n=0;n<r;n+=1)indent+=" ";else"string"==typeof r&&(indent=r);if(rep=e,e&&"function"!=typeof e&&("object"!=typeof e||"number"!=typeof e.length))throw new Error("JSON.stringify");return str("",{"":t})}),"function"!=typeof JSON.parse&&(JSON.parse=function(text,reviver){var j;function walk(t,e){var r,n,o=t[e];if(o&&"object"==typeof o)for(r in o)Object.prototype.hasOwnProperty.call(o,r)&&(void 0!==(n=walk(o,r))?o[r]=n:delete o[r]);return reviver.call(t,e,o)}if(text=String(text),rx_dangerous.lastIndex=0,rx_dangerous.test(text)&&(text=text.replace(rx_dangerous,function(t){return"\\u"+("0000"+t.charCodeAt(0).toString(16)).slice(-4)})),rx_one.test(text.replace(rx_two,"@").replace(rx_three,"]").replace(rx_four,"")))return j=eval("("+text+")"),"function"==typeof reviver?walk({"":j},""):j;throw new SyntaxError("JSON.parse")})}();


app.beginUndoGroup("Add new shape layer");  
  
var currComp = app.project.activeItem;  
 

var workouts;
var workoutData = new File('~/r/apple_watch_workouts/data/cycle_workouts.json');
if(workoutData.open("r")) {
    workoutData.encoding = "UTF-8";
    var workoutJSON = workoutData.read();
    var workouts = JSON.parse(workoutJSON);
    workoutData.close();
}

 
//starting positions for the first object.  
var xp = 1919.0;
var yp = 1042;
var zp = 25.0;
var zSpacing = 191.0;
var xSpacing = 489.0;


// sample workout objects that are displayed and animated as graphs.
/*
workout1 = {
        'mets':6.5, 'mi':30, 'mph':11

    }
    
workout2 = {
        'mets':5.4, 'mi':15, 'mph':14
    }    
    
    
workout3 = {
        'mets':9.1, 'mi':28, 'mph':7
    }      
    

var workouts = [workout1,workout2,workout3];
*/


for (var i = 0; i < workouts.length; i++) {
     if(currComp){ 
        
        
        var maxBarWidth = 200;
        var graphWidth = maxBarWidth;
        var textSpacingMargin = 10;
        var fontSize = 20;
        var startTime = 0;
        var animationLength = 2;        
        var linkTextToSliderExpression = "txt = thisLayer.effect('Slider Control')('Slider'); txt = parseFloat(txt).toPrecision(2); "
                
        
        aWorkout = workouts[i];
        u = getKeysWithoutObjectKeysSupport(aWorkout).sort();
        data = [aWorkout.mets, aWorkout.mi, aWorkout.mph];
        widths = data;
        
        
        if( (i > 0) && (i) % columnObjects == 0) {
            zp -= zSpacing * columnObjects;
            xp += xSpacing;
        }

        
        // build (3) graphs representing one workout.
        for (var j = 0; j < 3; j++){

             
            graph = currComp.layers.addShape();
            graphName = u[j]+' '+i+' graph';        
            graph.name = graphName;
            graph.threeDLayer = true;
            graph.materialOption.acceptsLights.setValue(0);                      
            graph.transform.position.setValue([ xp, yp, (zp + j*-25)]);            
            graph.anchorPoint.setValue([-(maxBarWidth/2),0,0]);
            graph.transform.xRotation.setValue(-90);
            var shapeGroup = graph.property("Contents").addProperty("ADBE Vector Group");  
            shapeGroup.property("Contents").addProperty("ADBE Vector Shape - Rect");          
            shapeGroup.property("Contents").addProperty("ADBE Vector Graphic - Fill");         
            shapeGroup.property("Contents").property("Rectangle Path 1").property("Size").setValue([maxBarWidth,15]);
            // sets scale animation
            scaleProp = graph.transform.scale;
            scaleProp.setValueAtTime(startTime,[0,100,100]);
            scaleProp.setValueAtTime(animationLength,[widths[j],100,100]);
             

            var text = currComp.layers.addText();
            var textYOffset = -6.5;
            text.name = u[j]+' '+i+' txt';
            text.threeDLayer = true;
            text.materialOption.acceptsLights.setValue(0);            
            text.transform.xRotation.setValue(-90);
            text.transform.position.expression = "thisComp.layer('"+graphName+"').transform.position"
            text.anchorPoint.setValue([0-textSpacingMargin,textYOffset,0]);
            textAnchorProp = text.anchorPoint;
            
            var textProperty = text.property("Source Text");
            var textPropertyValue = textProperty.value;
            textPropertyValue.fontSize = fontSize;
            textProperty.setValue(textPropertyValue);
            
            
            textAnchorProp.setValueAtTime(startTime, [0-textSpacingMargin,textYOffset,0]);
            textAnchorProp.setValueAtTime(animationLength, [-(graphWidth*(widths[j]/100))-textSpacingMargin,textYOffset ,0]);
    
            text.Effects.addProperty("ADBE Slider Control");
            sliderProp = text.effect("Slider Control")("Slider");
            sliderProp.setValueAtTime(startTime, 0.0);
            sliderProp.setValueAtTime(animationLength, data[j]);
            text.sourceText.expression = linkTextToSliderExpression + " text.sourceText = (txt + ' "+u[j]+"');";        
                      
        }
        
        zp += zSpacing;
                
    }  
}  


  
  
app.endUndoGroup(); 

