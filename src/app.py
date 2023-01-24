
import os
import requests
import flask
import traceback

import requests
import json
import pandas as pd

# Dash Framework
import dash_bootstrap_components as dbc
from dash import Dash, callback, clientside_callback, html, dcc, dash_table as dt, Input, Output, State, MATCH, ALL
from dash.exceptions import PreventUpdate

DATASTORE_URL = os.environ.get("DATASTORE_URL","url not found")
DATASTORE_URL = os.path.join(DATASTORE_URL, "api/")

server = flask.Flask('app')

# ---------------------------------
#   Get Data From datastore
# ---------------------------------

def get_api_data(api_address):
    api_json = {}
    try:
        try:
            response = requests.get(api_address)
        except:
            return('error: "requests.get failed"' )
        request_status = response.status_code
        if request_status == 200:
            api_json = response.json()
            return api_json
        else:
            return request_status
    except Exception as e:
        traceback.print_exc()
        api_json['json'] = 'error: {}'.format(e)
        return api_json


#
# print("data from datastore:", datafeed)




def basic_layout():
    api = 'consort'
    api_address = DATASTORE_URL + api
    api_json = get_api_data(api_address)
    if api_json:
        print('got api-json')
        child_div = json.dumps(api_json)
    else:
        print('no api-json')
        child_div = html.P('api_json failed')

    layout=html.Div([
        html.H2('Basic Layout'),
        html.Div(
            child_div
        )
    ])
    return layout

# ---------------------------------
#   build app
# ---------------------------------
external_stylesheets_list = [dbc.themes.SANDSTONE, 'https://codepen.io/chriddyp/pen/bWLwgP.css'] #  set any external stylesheets

app = Dash('app', server=server,
                external_stylesheets=external_stylesheets_list,
                suppress_callback_exceptions=True,
                meta_tags=[{'name': 'viewport', 'content': 'width=device-width, initial-scale=1'}])

app.layout = basic_layout #serve_layout


if __name__ == '__main__':
    app.run_server()


# ---------------------------------
#   Callbacks
# ---------------------------------




