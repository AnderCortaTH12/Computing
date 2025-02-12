    // Carga la biblioteca de Google Charts
    google.charts.load('current', {'packages':['corechart']});
    
    // Función para dibujar la gráfica cuando la biblioteca esté cargada
    google.charts.setOnLoadCallback(drawChart);
    
    // Función para dibujar la gráfica de barras
    function drawChart() {
        // Define los datos para la gráfica
        var data = google.visualization.arrayToDataTable(chartData);

        // Opciones de configuración de la gráfica
        var options = {
            title: 'Salary Progress',
            legend: { position: 'none' },
            hAxis: {
                title: 'Months'
            },
            vAxis: {
                title: 'Money in euro'
            }
        };

        // Crea la instancia de la gráfica de barras
        var chart = new google.visualization.ColumnChart(document.getElementById('googleChart'));

        // Dibuja la gráfica con los datos y opciones
        chart.draw(data, options);
    }
	