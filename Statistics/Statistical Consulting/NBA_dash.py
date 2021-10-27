#!/usr/bin/env python
# coding: utf-8

# Run this app with `python app.py` and
# visit http://127.0.0.1:8050/ in your web browser.
import pandas as pd
import dash
from dash import dcc
from dash import html
import plotly.express as px
import plotly.graph_objects as go

all_data = pd.read_csv('NBA_done.csv')
features = all_data.columns.to_list()[1:]

app = dash.Dash(__name__)
app.layout = html.Div([
    html.Div([
        html.Div([
            html.Label('Index 1'),
            dcc.Dropdown(
                id = 'index_for_x',
                options = [{'label': i, 'value': i} for i in features],
                value = 'ORtg'
            ),
            html.Label('Index 2'),
            dcc.Dropdown(
                id = 'index_for_y',
                options = [{'label': i, 'value': i} for i in features],
                value = 'DRtg'
            )],
        style = {'width': '49%', 'display': 'inline-block'}
        ),

        html.Div([
            html.Label('Conference'),
            dcc.Dropdown(
                id = 'conference',
                options = [
                    {'label': 'West', 'value': 'West'},  
                    {'label': 'East', 'value': 'East'},  
                    {'label': 'League Average', 'value': 'Average'} 
                ],
                value = ['West', 'East'],
                multi = True
            ),
            html.Label('Colors'),
            dcc.RadioItems(
                id = 'target_label',
                options = [
                    {'label': 'Age', 'value': 'Age'},
                    {'label': 'Salary', 'value': 'Salary'}
                ],
                value = 'Salary',
                labelStyle = {'display': 'inline-block', 'marginTop': '5px'}
            )
        ],
        style = {'width': '49%', 'display': 'inline-block', 'float': 'right'}
        )
    ],
    style = {'padding': '0px 10px 10px 0px'}
    ),

    html.Div([
        dcc.Graph(
            id = 'main_scatter',
            hoverData = {'points': [{'customdata': 'LA Clippers'}]}
        )
    ],
    style = {'width': '60%', 'display': 'inline-block', 'padding': '0'}
    ),

    html.Div([
        dcc.Graph(id = 'basic_stats'),
        dcc.Graph(id = 'shoot_stats')
    ],
    style = {'display': 'inline-block', 'width': '40%'}
    )
])

@app.callback(
    dash.dependencies.Output('main_scatter', 'figure'),
    [dash.dependencies.Input('index_for_x', 'value'),
    dash.dependencies.Input('index_for_y', 'value'),
    dash.dependencies.Input('target_label', 'value'),
    dash.dependencies.Input('conference', 'value')])
def update_graph(index_x, index_y, target, conference):
    league_avg = all_data[all_data['Team'] == 'League']
    select_data = all_data[all_data['Conference'].isin(conference)]
    fig = px.scatter(select_data, x = index_x, y = index_y, size = 'W', color = target, size_max = 35, hover_name = 'Team', text = 'Team', 
                    hover_data = [index_x, index_y, 'W', 'Salary'], title = f'{index_y} vs. {index_x} in 30 NBA Teams', height = 800)
    fig.update_traces(customdata = all_data['Team'])
    fig.add_vline(x = league_avg[index_x].values[0], line_width = 1)
    fig.add_hline(y = league_avg[index_y].values[0], line_width = 1)
    fig.update_xaxes(title = index_x)
    fig.update_yaxes(title = index_y)
    fig.update_layout(margin = {'l': 40, 'b': 50, 't': 30, 'r': 0}, hovermode = 'closest')
    return fig

@app.callback(
    dash.dependencies.Output('shoot_stats', 'figure'),
    [dash.dependencies.Input('main_scatter', 'hoverData')])
def basic_hist(hoverData):
    advance_ls = ['Team', 'FG%', '2P%', '3P%', 'FT%', 'TS%']
    data = all_data[advance_ls]
    target_row = data[data['Team'] == hoverData['points'][0]['customdata']]
    avg_row = data[data['Team'] == 'League']
    target_value = target_row.values[0][1:]
    avg_value = avg_row.values[0][1:]
    fig = go.Figure()
    fig.add_trace(go.Bar(x = advance_ls[1:], y = target_value*100, name = 'Selected Team', marker_color = 'red'))
    fig.add_trace(go.Bar(x = advance_ls[1:], y = avg_value*100, name = 'League Average', marker_color = 'lightsalmon'))
    fig.update_xaxes(title = target_row['Team'].values[0])
    fig.update_yaxes(title = 'Percentage (%)')
    fig.update_layout(title_text = 'Shooting Stats', legend = dict(orientation = 'h', y = 1.13))
    return fig

@app.callback(
    dash.dependencies.Output('basic_stats', 'figure'),
    [dash.dependencies.Input('main_scatter', 'hoverData')])
def basic_hist(hoverData):
    basic_ls = ['Team', 'ORB', 'DRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS']
    data = all_data[basic_ls]
    target_row = data[data['Team'] == hoverData['points'][0]['customdata']]
    avg_row = data[data['Team'] == 'League']
    target_value = target_row.values[0][1:]
    avg_value = avg_row.values[0][1:]
    fig = go.Figure()
    fig.add_trace(go.Bar(x = basic_ls[1:], y = target_value, name = 'Selected Team'))
    fig.add_trace(go.Bar(x = basic_ls[1:], y = avg_value, name = 'League Average', marker_color = 'lightblue'))
    fig.update_xaxes(title = target_row['Team'].values[0])
    fig.update_yaxes(title = 'Counts')
    fig.update_layout(title_text = 'Basic Stats', legend = dict(orientation = 'h', y = 1.13))
    return fig


if __name__ == '__main__':
    app.run_server(debug = True)








