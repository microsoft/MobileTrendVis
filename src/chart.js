var d3 = require("d3");
var flubber = require("flubber");
var globals = require("./globals");

d3.chart = function () {  

  /**

  GLOBAL CHART VARIABLES

  **/

  var params = {};
      params.yearMin = globals.param_yearMin;
      params.yearMax = globals.param_yearMax;      
      params.x = globals.param_x;
      params.y = globals.param_y;
      params.radius = globals.param_r;
      params.color = 'region';
      params.facet = 'facet';
      params.key = 'code';
  
  var scale_pop = d3.scaleLinear(), 
      scale_x = d3.scaleLinear(), 
      scale_y = d3.scaleLinear(), 
      scale_scroll = d3.scaleLinear(),
      scale_reg = d3.scaleOrdinal(),
      scale_tmp = d3.scaleLinear(),
      animation = 'off',
      loop_count = 0,
      animation_duration = (params.yearMax - params.yearMin) * 500,
      current_year = params.yearMin,
      scroll_year = current_year,
      facets = globals.facets, //show facets?
      num_facets = 0,
      num_facet_cols = 0,
      facet_dim = 0,
      lines = globals.lines, //show lines?
      this_chart,
      thousand_format = d3.format(".2s"),
      caption_text = "";

  function chart (selection) {
    selection.each(function (data){

      this_chart = d3.select(this);
      this_chart.interrupt();

      /**

      DATA WRANGLING

      **/

      //sort data alphabetically by country code
      data.sort(function(x, y){
        return d3.ascending(x[params.key], y[params.key]);
      });

      //assign facet index
      data.forEach(function(d,i){
        d[params.facet] = i;
      });

      num_facets = d3.max(data, function(d){
        return d[params.facet] + 1;
      });

      num_facet_cols = Math.ceil(Math.sqrt(num_facets));

      facet_dim = chart_dim / num_facet_cols;      

      var countries = data.map(function(d){
        return {
          id: d.code,
          region: d.region,
          facet: d.facet,
          param_x: d[params.x].map(function(d) {
            return d[1];
          }),
          param_y: d[params.y].map(function(d) {
            return d[1];
          })
        };
      });

      countries.forEach(function(d){
        d.values = [];
        for(var i = 0; i < d.param_x.length; i++){
          if (d.param_x[i] != null && d.param_y[i] != null) {
            d.values.push({
              'param_x': d.param_x[i],
              'param_y': d.param_y[i]
            });
          }
        }
      }); 
      
      /**

      SCALES: initialize domains and ranges

      **/

      //region scale (color)

      scale_reg.range([
        "#fad139",
        "#54b2fc",
        "#f67afe",
        "#8bba32",
        "#c29aeb"]

      );

      //size scale

      if (facets == 'on') {
        scale_pop.range([1.75,7]); // r scale range      
      }
      else {
        scale_pop.range([7,28]); // r scale range      
      }
      
      var pop_max = d3.max(data, function(d){
        return d3.max(d[params.radius], function (dd) {
          return Math.sqrt(dd[1]);
        });
      });
      
      var pop_buffer = pop_max * 0.05;
      
      scale_pop.domain([0,pop_max + pop_buffer]);
      scale_pop.nice();

      //y scale

      if (facets == 'on') {
        scale_y.range([facet_dim, 0]); // y scale range
      }
      else {
        scale_y.range([chart_dim, 0]); // y scale range
      }
      
      var y_min = d3.min(data, function(d){
        return d3.min(d[params.y], function (dd){
          return dd[1];
        });
      });
      
      var y_max = d3.max(data, function(d){
        return d3.max(d[params.y], function (dd) {
          return dd[1];
        });
      });
      
      var y_buffer = (y_max - y_min) * 0.05;
      
      scale_y.domain([d3.max([0,y_min - y_buffer]),y_max + y_buffer]);
      scale_y.nice();

      
      //x scale
      
      if (facets == 'on') {
        scale_x.range([0, facet_dim]); // x scale range
      }
      else {
        scale_x.range([0, chart_dim]); // x scale range
      }
      
      var x_min = d3.min(data, function(d){
        return d3.min(d[params.x], function (dd){
          return dd[1];
        });
      });
      
      var x_max = d3.max(data, function(d){
        return d3.max(d[params.x], function (dd) {
          return dd[1];
        });
      });
      
      var x_buffer = (x_max - x_min) * 0.05;
      
      scale_x.domain([d3.max([0,x_min - x_buffer]),x_max + x_buffer]);
      scale_x.nice();

      // time scale

      scale_tmp.range([-5,5]);
      scale_tmp.domain([-0.5,0.5]);
      scale_tmp.clamp(true);
      scale_tmp.nice();
      scale_tmp.tickFormat(d3.format("d"));

      //SCROLL RING SCALE

      scale_scroll.range([-(4/12),(4/12)]);
      scale_scroll.domain([-10,10]);
      scale_scroll.clamp(true);

      /** 
       
      DEFS: add as required 
      
      **/
     
      /** 
      
        GUIDES / AXES

      **/

      year_indicator = this_chart.selectAll('.year_indicator')
      .data([null]);

      year_indicator.enter()
      .append("text")
      .attr("class", "year_indicator");

      year_indicator.transition().duration(250)
      .attr('id', facets == 'on' ? "faceted_year_indicator" : (current_year == params.yearMin) ? "central_year_indicator_min" : "central_year_indicator")
      .attr('text-anchor', facets == 'on' ? "middle" : "middle")
      .attr('alignment-baseline',facets == 'on' ?  'baseline' : 'middle')
      .attr('dy',facets == 'on' ?  '-0.2em' : '0em')
      .attr('transform', function () {
        return facets == 'on' ? 'translate(' + (chart_dim / 2) + ',' + (0 - 0.5 * inner_padding) + ')' : 'translate(' + (chart_dim / 2) + ',' + (chart_dim / 2) + ')';
      })
      .text(facets == 'on' ? (params.yearMin + ' ― ' + params.yearMax) : Math.round(current_year));

      year_indicator.exit()
      .remove();

      var x_indicator = this_chart.selectAll('.x_indicator')
      .data([null]);

      var x_indicator_enter = x_indicator.enter()
      .append("g")
      .attr("class", "x_indicator");

      x_indicator_enter.append('line');

      x_indicator.select('line').attr('x1', 0)
      .attr('x2', chart_dim)
      .attr('y1', chart_dim)
      .attr('y2', chart_dim)
      .style("stroke", facets == 'on' ? 'none' : '#ccc');

      x_indicator_enter.append("text")
      .attr('class','indicator_text');

      x_indicator.select('text').attr('text-anchor', "middle")
      .attr('alignment-baseline','hanging')
      .attr('dy','0.2em')
      .text('← ' + params.x + ' →')
      .attr('transform', function () {
        return 'translate(' + (chart_dim / 2) + ',' + (chart_dim + inner_padding * 0.5) + ')';
      });

      x_indicator.exit()
      .remove();

      var y_indicator = this_chart.selectAll('.y_indicator')
      .data([null]);

      var y_indicator_enter = y_indicator.enter()
      .append("g")
      .attr("class", "y_indicator");

      y_indicator_enter.append('line');

      y_indicator.select('line')
      .attr('x1', 0)
      .attr('x2', 0)
      .attr('y1', 0)
      .attr('y2', chart_dim)
      .style("stroke", facets == 'on' ? 'none' : '#ccc');

      y_indicator_enter.append("text")
      .attr('class','indicator_text');

      y_indicator.select('text')
      .attr('text-anchor', "middle")
      .attr('alignment-baseline','baseline')
      .attr('dy','-0.6em')
      .text('← ' + params.y + ' →')
      .attr('transform', function () {
        return 'translate(' + (0 - inner_padding * 0.5) + ',' + (chart_dim / 2) + ')rotate(-90)';
      });

      y_indicator.exit()
      .remove();

      
      /**
        
        HELPER FUNCTIONS
        
      **/

      var bisect = d3.bisector(function(d){
        return d[0];
      });

      function interpolateData(year) {
        return data.map(function (d){
          var tmp = {};
            tmp[params.key] = d[params.key];
            tmp[params.facet] = d[params.facet];
            tmp[params.color] = d[params.color];
            tmp[params.x] = (interpolateValues(d[params.x], year) != null) ? interpolateValues(d[params.x], year) : x_min;
            tmp[params.radius] = (interpolateValues(d[params.radius], year) != null) ? interpolateValues(d[params.radius], year) : 0;
            tmp[params.y] = (interpolateValues(d[params.y], year) != null) ? interpolateValues(d[params.y], year) : y_min;
            return tmp;
        });
      }

      // Finds (and possibly interpolates) the value for the specified year.
      function interpolateValues(values, year) {
        var i = bisect.left(values, year, 0, values.length - 1),
                a = values[i];
        if (i > 0) {
            var b = values[i - 1],
                    t = (year - a[0]) / (b[0] - a[0]);
            return a[1] * (1 - t) + b[1] * t;
        }
        return a[1];
      }      

      function order(a, b) {
        return b[params.radius] - a[params.radius];
      }

      // Positions the marks based on data.
      function position(mark) {        

        var mark_transition;
        
        if (animation == 'off'){
          mark_transition = mark.transition()
          .delay(function(d,i){
            return i * 10;
          })
          .duration(500);
        }
        else if (animation == 'on'){
          mark_transition = mark.transition()         
          .delay(function(d,i){
            return i * (50 / data.length);
          })
          .duration(50);
        }
        else {
          mark_transition = mark;
        }

        mark_transition.select('circle')
        .attr("cx", function (d) {         
          return (d[params.x] != null) ? scale_x(d[params.x]) : - facet_dim;
        })        
        .style("stroke", function(d){
          if (facets == 'on') {
            return '#999';
          }                   
          else {
            return '#fff';
          }
        })
        .style("opacity", function(d){            
          if (facets == 'on') {
            d3.select('#line_'+ d[params.key]).select('path').style('opacity',1);
            d3.select('#text_mark_' + d[params.key]).attr("class", 'text_mark');
            
            return 1;              
          }
          else {             
            
            d3.select('#line_'+ d[params.key]).select('path').style('opacity',0.5);
            d3.select('#text_mark_' + d[params.key]).attr("class", 'text_mark');
            
            return 0.5;
          }            
        })
        .attr("cy", function (d) {
          return d[params.y] != null ? scale_y(d[params.y]) : - facet_dim;
        })
        .attr("r", function (d) {
          return (d[params.y] != null && d[params.x] != null) ? scale_pop(Math.sqrt(d[params.radius])) : 0;
        })
        .attr('transform', function (d) {
          if (facets == 'on') {
            return 'translate(' + (facet_dim * (d[params.facet] % num_facet_cols)) + ',' + (facet_dim * Math.floor(d[params.facet] / num_facet_cols)) + ')';
          }
          else {
            return 'translate(0,0)';
          }
        }); 
        
        mark_transition.select('text')
        .attr("x", function (d) {         
          return facets == 'on' ? 0 : ((d[params.x] != null) ? scale_x(d[params.x]) : - facet_dim);
        })
        .attr("dy", facets == "on" ? '-1em' : '0em')        
        
        .attr("y", function (d) {
          return facets == 'on' ? 0 : (d[params.y] != null ? scale_y(d[params.y]) : - facet_dim);
        })
        .attr('transform', function (d) {
          if (facets == 'on') {
            return 'translate(' + (facet_dim * (d[params.facet] % num_facet_cols) + facet_dim * 0.5) + ',' + (facet_dim * Math.floor(d[params.facet] / num_facet_cols) + facet_dim) + ')';
          }
          else {
            return 'translate(0,0)';
          }
        });        
      }      

      function tweenYear() {
        var year = d3.interpolateNumber(params.yearMin,params.yearMax);
        var counter = 0;
        return function (t) {
          now = new Date();
          if (counter != Math.floor((t * animation_duration) / 100)) {
            displayYear(year(t));
          } 
          counter = Math.floor((t * animation_duration) / 100);
        };
      }

      function tweenCurrentYear() {
        var year = d3.interpolateNumber(current_year,params.yearMax);
        var counter = 0;
        return function (t) {
          now = new Date();
          if (counter != Math.floor((t * animation_duration) / 100)) {
            displayYear(year(t));
          } 
          counter = Math.floor((t * animation_duration) / 100);
        };
      }

      function displayYear(year) {        
       
        var progress =  ((year + 1) - params.yearMin) / ((params.yearMax + 1) - params.yearMin);
        if (globals.outer_progress_circle != undefined) {
          d3.select('#outer_progress_value').attr('d', globals.outer_progress_circle.endAngle((Math.PI * 2) * (loop_count + progress)));
        }
        
        current_year = year;
        circle_marks.data(interpolateData(year), function(d){
          return d[params.key];
        })
        .call(position)
        .sort(order);               
        
        d3.select('.year_indicator').text(facets == 'on' ? (params.yearMin + ' ― ' + params.yearMax) : Math.round(current_year));
        
      }

      var line = d3.line()
      .curve(d3.curveBasis)
      .x(function(d) { 
        return scale_x(d.param_x); 
      })
      .y(function(d) { 
        return scale_y(d.param_y); 
      });      

      function repeat() {
        if (animation == 'on') {            

          animation_duration = (params.yearMax - params.yearMin) * 500;
          this_chart.transition()
          .duration(animation_duration)
          .tween('year',tweenYear)
          .ease(d3.easeLinear)
          .on('end',function(){
            if (globals.trial_index > -1 && globals.num_selected == globals.trials[globals.trial_index].num_responses){
              d3.select('#progress_indicator').style('display','none');
              d3.select('#done_btn').attr('class','img_btn_enabled')
              .style('display',null)
              .attr('disabled',null)
              .attr('src', 'assets/done.svg');
            }
            else {
              d3.select('#submit_btn').attr('class','menu_btn_enabled')
              .attr('disabled',null);
            }
            loop_count++;
            if (!introduction_complete && globals.num_selected == 3) {              
              d3.select('#progress_indicator').style('display','none');
              d3.select('#done_btn').attr('class','img_btn_enabled')
              .style('display',null)
              .attr('disabled',null)
              .attr('src', 'assets/done.svg');
            }
            else {
              if (introduction_complete && loop_count > 0 && globals.num_selected == globals.trials[globals.trial_index].num_responses){
                d3.select('#done_btn').attr('class','img_btn_enabled')
                .attr('src', 'assets/done.svg');
              }
            }
            repeat();
          });
        }
        else {
          displayYear(current_year);
        }         
      }

      /**

      DATA ELEMENT ENTER

      **/

      // CIRCLES

      var circle_marks = this_chart.selectAll(".mark")
      .data(interpolateData(params.yearMin), function(d) {
        return d[params.key];
      });      

      // FACETS

      var facet_bounds = this_chart.selectAll(".facet")
      .data(data, function(d) {
        return d[params.key];
      });

      var facet_bound_enter = facet_bounds.enter()
      .append("g")
      .attr("class","facet")
      .attr("id", function (d) {
        return "facet_" + d[params.key];
      });
  
      facet_bound_enter.append('rect')
      .attr("class","facet_rect")      
      .attr("id", function (d) {
        return "facet_" + d[params.key];
      })
      .style("fill", facets == 'on' ? 'transparent' : 'none')
      .attr('width', facet_dim)
      .attr('height', facet_dim)
      .style("stroke-dasharray", '0.1em')
      .style("stroke", facets == 'on' ? '#666' : 'none')
      .attr('transform', function (d) {
        return 'translate(' + (facet_dim * (d[params.facet] % num_facet_cols)) + ',' + (facet_dim * Math.floor(d[params.facet] / num_facet_cols)) + ')';
      })
      .attr('rx', 5);             

      // LINES
            
      var line_marks = this_chart.selectAll(".line")
      .data(countries, function(d) {
        return d.id;
      });

      var line_mark_enter = line_marks.enter()
      .append("g")
      .attr("class","line")
      .attr("id", function (d) {
        return "line_" + d.id;
      });
  
      line_mark_enter.append('path')
      .attr("class","path_line")      
      .style("stroke", function (d) {
        return lines == 'on' ? scale_reg(d[params.color]) : 'transparent';
      })
      .style('fill','none')
      .attr("d", function(d) { 
        var tmp = d.values;           
        return line(tmp); 
      })
      .style("opacity", function(){
        if (facets == 'on') {
          return 1;
        }
        else {
          return 0.5;
        }
      })
      .attr('transform', function (d) {
        if (facets == 'on') {
          return 'translate(' + (facet_dim * (d[params.facet] % num_facet_cols)) + ',' + (facet_dim * Math.floor(d[params.facet] / num_facet_cols)) + ')';
        }
        else {
          return 'translate(0,0)';
        }
      });   
      
      //CIRCLE ENTER
      
      var circle_mark_enter = circle_marks.enter()
      .append("g")
      .attr("class","mark")
      .attr("id", function (d) {
        return "mark_" + d[params.key];
      });

      circle_mark_enter.append('circle')
      .attr("class", function (d) {
        return "circle_mark_" + d[params.key];
      })   
      .style("opacity", function(d){
        if (facets == 'on') {
          return 1;
        }
        else {
          return 0.5;
        }
      })
      .style("stroke", function(d){
        if (facets == 'on') {
          return '#999';
        }             
        else {
          return '#fff';
        }
      })
      .style("fill", function (d) {       
        return scale_reg(d[params.color]);
      });

      circle_mark_enter.append('text')
      .attr("class", 'text_mark')
      .attr('text-anchor', "middle")
      .attr('alignment-baseline','middle')
      .attr('id', function(d){
        return "text_mark_" + d[params.key];
      })
      .style('display',null)
      .text(function(d){
        return d[params.key];
      });

      circle_marks.call(position)
      .sort(order);  
      
      /**

      DATA ELEMENT UPDATE 

      **/

      repeat();    
      
      var facet_bound_update = facet_bounds.transition()
      .delay(function(d,i){
        return i * 10;
      })
      .duration(500);
      
      facet_bound_update.selectAll('rect.facet_rect')      
      .style("stroke", function(d){
        return facets == 'on' ? '#999' : 'none';              
      })
      .style("fill", facets == 'on' ? 'transparent' : 'none')
      .attr('transform', function (d) {
        return 'translate(' + (facet_dim * (d[params.facet] % num_facet_cols)) + ',' + (facet_dim * Math.floor(d[params.facet] / num_facet_cols)) + ')';
      });

      var line_mark_update = line_marks.transition()
      .delay(function(d,i){
        return i * 10;
      })
      .duration(500);

      line_mark_update.selectAll('path.path_line')
      .style("stroke", function (d) {
        return lines == 'on' ? scale_reg(d[params.color]) : 'transparent';
      })
      .attr("d", function(d) { 
        var tmp = d.values;   
        // for (var i = 0; i < tmp.length; i++) {
        //   if (tmp[i].param_x == null || tmp[i].param_y == null){
        //     tmp.splice(i,1);
        //   }
        // }
        // console.log(tmp);
        return line(tmp); 
      })
      .attr('transform', function (d) {
        if (facets == 'on') {
          return 'translate(' + (facet_dim * (d[params.facet] % num_facet_cols)) + ',' + (facet_dim * Math.floor(d[params.facet] / num_facet_cols)) + ')';
        }
        else {
          return 'translate(0,0)';
        }
      }); 
      
      /**

      DATA ELEMENT EXIT

      **/

      circle_marks.exit()      
      .remove();

      line_marks.exit()
      .remove();

      facet_bounds.exit()
      .remove();
    
    });
  }    

  /**

  GETTER / SETTER FUNCTIONS

  **/  

  //getter / setter for showing lines
  chart.lines = function (x) {
    if (!arguments.length) {
      return lines;
    }
    lines = x;
    return chart;
  };

  //getter / setter for animation
  chart.animation = function (x) {
    if (!arguments.length) {
      return animation;
    }
    animation = x;
    return chart;
  };

  //getter / setter for facets
  chart.facets = function (x) {
    if (!arguments.length) {
      return facets;
    }
    facets = x;
    return chart;
  };

  //getter / setter for current_year
  chart.current_year = function (x) {
    if (!arguments.length) {
      return current_year;
    }
    current_year = x;
    return chart;
  };
  
  //getter / setter for scale_pop
  chart.scale_pop = function (x) {
    if (!arguments.length) {
      return scale_pop;
    }
    scale_pop = x;
    return chart;
  };

  //getter / setter for scale_x
  chart.scale_x = function (x) {
    if (!arguments.length) {
      return scale_x;
    }
    scale_x = x;
    return chart;
  };

  //getter / setter for scale_y
  chart.scale_y = function (x) {
    if (!arguments.length) {
      return scale_y;
    }
    scale_y = x;
    return chart;
  };

  //getter / setter for scale_reg
  chart.scale_reg = function (x) {
    if (!arguments.length) {
      return scale_reg;
    }
    scale_reg = x;
    return chart;
  };

  //getter / setter for scale_tmp
  chart.scale_tmp = function (x) {
    if (!arguments.length) {
      return scale_tmp;
    }
    scale_tmp = x;
    return chart;
  };

  //getter / setter for scale_scroll
  chart.scale_scroll = function (x) {
    if (!arguments.length) {
      return scale_scroll;
    }
    scale_scroll = x;
    return chart;
  };

  //getter / setter for params
  chart.params = function (x) {
    if (!arguments.length) {
      return params;
    }
    params = x;
    return chart;
  };

  //getter / setter for this_chart
  chart.this_chart = function (x) {
    if (!arguments.length) {
      return this_chart;
    }
    this_chart = x;
    return chart;
  };
  
  //getter / setter for loop_count
  chart.loop_count = function (x) {
    if (!arguments.length) {
      return loop_count;
    }
    loop_count = x;
    return chart;
  };

  //getter / setter for caption_text
  chart.caption_text = function (x) {
    if (!arguments.length) {
      return caption_text;
    }
    caption_text = x;
    return chart;
  };
 
  return chart;

};

module.exports = d3.chart;