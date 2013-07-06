var margin = {top: 40, right: 10, bottom: 10, left: 10},
    width = 1500 - margin.left - margin.right,
    height = 900 - margin.top - margin.bottom;

var x = d3.scale.ordinal()
          .rangeRoundBands([0, width], .1);

var y = d3.scale.ordinal()
          .rangeRoundBands([0, height], .1);

var color = d3.scale.category20c();

var svg = d3.select("body")
            .append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("transform", "translate(" + margin.left + ", " + margin.top + ")");

var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
var ws = new Socket('ws://localhost:9160/');

ws.onmessage = function(event){
  var json = eval("(" + event.data + ")");
  var nodes = json.nodes;
  var links = json.links;

  x.domain(d3.range(json.window_width));
  y.domain(d3.range(json.line_min, json.line_max+1));

  svg.selectAll(".label")
     .data(nodes)
     .enter()
     .append("rect")
     .attr("class", "label")
     .attr("x", function(d){ return x(d.col) })
     .attr("y", function(d){ return y(d.line) })
     .attr("width", function(d){ return x(d.col + d.length) - x(d.col) })
     .attr("height", function(d){ return y.rangeBand() })
     .style("fill", function(d){
       if(d.count == 1){
         return "#F00";
       }else if(d.count == 2){
         return "#0F0";
       }else if(d.count == 3){
         return "#00F";
       }else{
         return "#FFF";
       }
     });
};

d3.select('#send').on('click', function(){
    ws.send("MSG1");
});
