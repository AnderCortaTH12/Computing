import pandas as pd
import numpy as np
import pyodbc
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from keras.models import Sequential
from keras.layers import LSTM, Dense
import tensorflow as tf
import random
import os

# === PARÁMETROS AJUSTABLES ===
EPOCHS = 80
BATCH_SIZE = 16
WINDOW_SIZE = 10
SEEDS = [42, 327, 3363, 6762, 8445]

# === FUNCIONES AUXILIARES ===
def set_seed(seed):
    np.random.seed(seed)
    random.seed(seed)
    tf.random.set_seed(seed)
    os.environ['PYTHONHASHSEED'] = str(seed)

def prepare_lstm_data(df, features, target, n_steps):
    scaler = MinMaxScaler()
    data = df[features + [target]].copy()
    scaled = scaler.fit_transform(data)
    X, y = [], []
    for i in range(n_steps, len(scaled)):
        X.append(scaled[i - n_steps:i, :-1])
        y.append(scaled[i, -1])
    return np.array(X), np.array(y), scaler

def split_data(X, y, train_frac=0.7, val_frac=0.15):
    n = len(X)
    train_end = int(n * train_frac)
    val_end = int(n * (train_frac + val_frac))
    return (
        X[:train_end], y[:train_end],
        X[train_end:val_end], y[train_end:val_end],
        X[val_end:], y[val_end:]
    )

def build_lstm_model(input_shape):
    model = Sequential()
    model.add(LSTM(50, activation='relu', input_shape=input_shape))
    model.add(Dense(1))
    model.compile(optimizer='adam', loss='mse')
    return model

def inverse_transform_predictions(scaler, y_pred, y_real):
    n_total_features = scaler.n_features_in_
    pad_pred = np.zeros((len(y_pred), n_total_features))
    pad_true = np.zeros((len(y_real), n_total_features))
    pad_pred[:, -1] = y_pred.flatten()
    pad_true[:, -1] = y_real.flatten()
    inverse_pred = scaler.inverse_transform(pad_pred)[:, -1]
    inverse_true = scaler.inverse_transform(pad_true)[:, -1]
    return inverse_true, inverse_pred

def evaluate_model(X_train, y_train, X_val, y_val, X_test, y_test, scaler):
    model = build_lstm_model((X_train.shape[1], X_train.shape[2]))
    model.fit(X_train, y_train, validation_data=(X_val, y_val), epochs=EPOCHS, batch_size=BATCH_SIZE, verbose=0)

    y_test_pred = model.predict(X_test)
    inv_test, inv_test_pred = inverse_transform_predictions(scaler, y_test_pred, y_test)

    # Cálculo naive (y_t = y_{t-1})
    inv_test_naive = inv_test[:-1]
    inv_test_real = inv_test[1:]
    inv_test_pred_shifted = inv_test_pred[1:]
    naive_mae = mean_absolute_error(inv_test_real, inv_test_naive)

    mae = mean_absolute_error(inv_test, inv_test_pred)
    rmse = np.sqrt(mean_squared_error(inv_test, inv_test_pred))
    r2 = r2_score(inv_test, inv_test_pred)
    return mae, rmse, r2, naive_mae

# === CONEXIÓN Y PREPARACIÓN DE DATOS ===
db_path = r'Base_de_datos\\Articulos_sentimiento_bolsa.accdb'
conn_str = (
    r'DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};'
    rf'DBQ={db_path};'
)
conn = pyodbc.connect(conn_str)
df = pd.read_sql("SELECT * FROM AAPL_cierre_filtrado", conn)
conn.close()

df = df.sort_values(by='Fecha').dropna()

features_A = ['Close', 'MediaMovil20', 'MediaMovil60']
features_B = features_A + ['media_movil_5dias', 'media_movil_20dias']
target = 'Close'

# === BUCLE PRINCIPAL ===
results = []

for seed in SEEDS:
    print(f"\n🔁 Ejecutando con semilla: {seed}")
    set_seed(seed)

    # Modelo A
    X_a, y_a, scaler_a = prepare_lstm_data(df, features_A, target, WINDOW_SIZE)
    X_a_train, y_a_train, X_a_val, y_a_val, X_a_test, y_a_test = split_data(X_a, y_a)
    mae_a, rmse_a, r2_a, naive_a = evaluate_model(X_a_train, y_a_train, X_a_val, y_a_val, X_a_test, y_a_test, scaler_a)

    # Modelo B
    X_b, y_b, scaler_b = prepare_lstm_data(df, features_B, target, WINDOW_SIZE)
    X_b_train, y_b_train, X_b_val, y_b_val, X_b_test, y_b_test = split_data(X_b, y_b)
    mae_b, rmse_b, r2_b, naive_b = evaluate_model(X_b_train, y_b_train, X_b_val, y_b_val, X_b_test, y_b_test, scaler_b)

    results.append({
        "Seed": seed, "Modelo": "A (sin sentimiento)",
        "MAE": mae_a, "RMSE": rmse_a, "R2": r2_a, "MAE_naive": naive_a
    })
    results.append({
        "Seed": seed, "Modelo": "B (con sentimiento)",
        "MAE": mae_b, "RMSE": rmse_b, "R2": r2_b, "MAE_naive": naive_b
    })

# === MOSTRAR RESULTADOS EN TABLA ===
results_df = pd.DataFrame(results)
print("\n📋 Resultados con métricas extendidas:\n")
print(results_df.pivot(index='Seed', columns='Modelo', values=['MAE', 'RMSE', 'R2', 'MAE_naive']).round(4))
