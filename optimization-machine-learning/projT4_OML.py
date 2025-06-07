import numpy as np
import matplotlib.pyplot as plt
import time

# Função de Horton (acumulada)
def F_horton(t, f0, fc, k):
    k = k if abs(k) > 1e-8 else 1e-8  # evita divisão por zero
    exp_term = np.exp(np.clip(-k * t, -100, 100))
    return fc * t + (f0 - fc) / k * (1 - exp_term)

# Função de custo (Erro Quadrático Médio)
def J(f0, fc, k, t, y):
    F = F_horton(t, f0, fc, k)
    return np.mean((F - y)**2)

# Gradiente da função de custo
def grad_J(f0, fc, k, t, y):
    F = F_horton(t, f0, fc, k)
    err = F - y
    exp_term = np.exp(np.clip(-k * t, -100, 100))  # ← PREVENÇÃO DE OVERFLOW
    dF_df0 = (1 - exp_term) / k if k != 0 else t
    dF_dfc = t - (1 - exp_term) / k if k != 0 else 0
    dF_dk = (f0 - fc) * (t * exp_term - (1 - exp_term)/k) if k != 0 else 0
    dJ_df0 = 2 * np.mean(err * dF_df0)
    dJ_dfc = 2 * np.mean(err * dF_dfc)
    dJ_dk  = 2 * np.mean(err * dF_dk)
    return np.array([dJ_df0, dJ_dfc, dJ_dk])

#--------------------------------------------Método Gradiente------------------------------------------------------------------

# Método do gradiente com histórico de erro
def gradiente_com_erro(f0_init, fc_init, k_init, t, y, alpha=1e-7, tol=1e-8, max_iter=10000):
    w = np.array([f0_init, fc_init, k_init])
    erros = []
    for i in range(max_iter):
        erro_atual = J(w[0], w[1], w[2], t, y)
        erros.append(erro_atual)
        grad = grad_J(w[0], w[1], w[2], t, y)
        w_new = w - alpha * grad
        if np.linalg.norm(w_new - w) < tol:
            break
        w = w_new
    return w, erros

#--------------------------------------------Método BFGS------------------------------------------------------------------

# Método BFGS com histórico de erro (formulação teórica)
def bfgs_com_erro(f0_init, fc_init, k_init, t, y, tol=1e-8, max_iter=10000):
    w = np.array([f0_init, fc_init, k_init], dtype=float)
    B = np.eye(3)  # aproximação inicial da Hessiana
    erros = []

    for k in range(max_iter):
        grad = grad_J(w[0], w[1], w[2], t, y)
        erro_atual = J(w[0], w[1], w[2], t, y)
        erros.append(erro_atual)

        if np.linalg.norm(grad) < tol:
            break

        # Direção de descida
        s = -np.linalg.solve(B, grad)

        # Atualização dos parâmetros: w^{(k+1)} = w^{(k)} + η^{(k)} s^{(k)}
        alpha = linha_de_pesquisa_wolfe(w, s, t, y, grad)
        w_new = w + alpha * s

        # Gradiente no novo ponto
        grad_new = grad_J(w_new[0], w_new[1], w_new[2], t, y)

        # Cálculo das diferenças
        yk = grad_new - grad
        sk = w_new - w

        # Atualização de B^{(k+1)} com fórmula BFGS
        if np.dot(sk, yk) > 0:  # condição de curvatura
            rho = 1.0 / np.dot(yk, sk)
            term1 = np.outer(yk, yk) * rho
            term2 = B @ np.outer(sk, sk) @ B / (sk.T @ B @ sk)
            B = B + term1 - term2

        w = w_new

    return w, erros

def linha_de_pesquisa_wolfe(w, s, t, y, grad, c1=1e-4, c2=0.9, alpha_init=0.0001):
    alpha = alpha_init
    max_iter = 50
    phi0 = J(w[0], w[1], w[2], t, y)
    phi0_prime = np.dot(grad, s)

    for _ in range(max_iter):
        w_new = w + alpha * s
        phi = J(w_new[0], w_new[1], w_new[2], t, y)
        grad_new = grad_J(w_new[0], w_new[1], w_new[2], t, y)
        phi_prime = np.dot(grad_new, s)

        if phi > phi0 + c1 * alpha * phi0_prime:
            alpha *= 0.5  # reduz passo (não cumpre Armijo)
        elif phi_prime < c2 * phi0_prime:
            alpha *= 1.1  # aumenta passo (não cumpre curvatura)
        else:
            break  # cumpre ambas → aceita
    return alpha

#--------------------------------------------Método Newton------------------------------------------------------------------

# Hessiana da função de custo
def hessiana_J(f0, fc, k, t, y):
    exp_term = np.exp(-k * t)
    err = F_horton(t, f0, fc, k) - y

    # Derivadas primeiras de F
    dF_df0 = (1 - exp_term) / k if k != 0 else t
    dF_dfc = t - (1 - exp_term) / k if k != 0 else 0
    dF_dk = (f0 - fc) * (t * exp_term - (1 - exp_term) / k) if k != 0 else 0

    # Derivadas segundas (apenas algumas têm expressão significativa)
    d2F_df0df0 = np.zeros_like(t)
    d2F_dfcdfc = np.zeros_like(t)
    d2F_dkdk = (f0 - fc) * (-t**2 * exp_term + 2 * t * exp_term / k - 2 * (1 - exp_term) / k**2) if k != 0 else 0
    d2F_df0dfc = np.zeros_like(t)
    d2F_df0dk = (-t * exp_term + (1 - exp_term) / k) / k if k != 0 else 0
    d2F_dfcdk = (-t * exp_term + (1 - exp_term) / k) / k if k != 0 else 0

    # Elementos da Hessiana de J (segunda derivada do EQM)
    dJ_df0df0 = 2 * np.mean(dF_df0**2 + err * d2F_df0df0)
    dJ_dfcdfc = 2 * np.mean(dF_dfc**2 + err * d2F_dfcdfc)
    dJ_dkdk = 2 * np.mean(dF_dk**2 + err * d2F_dkdk)
    dJ_df0dfc = 2 * np.mean(dF_df0 * dF_dfc + err * d2F_df0dfc)
    dJ_df0dk = 2 * np.mean(dF_df0 * dF_dk + err * d2F_df0dk)
    dJ_dfcdk = 2 * np.mean(dF_dfc * dF_dk + err * d2F_dfcdk)

    # Construção da matriz Hessiana simétrica
    H = np.array([
        [dJ_df0df0, dJ_df0dfc, dJ_df0dk],
        [dJ_df0dfc, dJ_dfcdfc, dJ_dfcdk],
        [dJ_df0dk,  dJ_dfcdk,  dJ_dkdk]
    ])
    
    return H

def newton_com_erro(f0_init, fc_init, k_init, t, y, tol=1e-8, max_iter=1):
    w = np.array([f0_init, fc_init, k_init], dtype=float)
    erros = []

    for i in range(max_iter):
        erro_atual = J(w[0], w[1], w[2], t, y)
        erros.append(erro_atual)

        grad = grad_J(w[0], w[1], w[2], t, y)
        H = hessiana_J(w[0], w[1], w[2], t, y)

        if np.linalg.norm(grad) < tol:
            break

        # Direção de Newton: H s = -grad
        
        s = -np.linalg.solve(H, grad)
        
        # Passo
        alpha = 1
        w_new = w + alpha * s
        w_new[1] = max(w_new[1], 0)    # fc >= 0

        if np.linalg.norm(w_new - w) < tol:
            break

        w = w_new

    return w, erros

# Condição de Armijo
def linha_de_pesquisa_armijo(w, s, t, y, grad, c1=1e-4, eta_init=1.0):
    eta = eta_init
    phi0 = J(*w, t, y)
    phi0_prime = np.dot(grad, s)
    for _ in range(20):
        w_novo = w + eta * s
        if J(*w_novo, t, y) <= phi0 + c1 * eta * phi0_prime:
            return eta
        eta *= 0.5
    return eta

def newtonarmijo_com_erro(f0_init, fc_init, k_init, t, y, max_iter=1, tol=1e-8):
    w = np.array([f0_init, fc_init, k_init], dtype=float)
    erros = []
    convergiu = False
    for i in range(max_iter):
        erro_atual = J(*w, t, y)
        erros.append(erro_atual)
        grad = grad_J(*w, t, y)
        if np.linalg.norm(grad) < tol:
            convergiu = True
            return w, erros
        H = hessiana_J(*w, t, y)
        try:
            s = -np.linalg.solve(H, grad)
        except np.linalg.LinAlgError:
            print("Hessiana não invertível.")
            break
        alpha = linha_de_pesquisa_armijo(w, s, t, y, grad)
        w_new = w + alpha * s
        w_new[1] = max(w_new[1], 0)
        if np.linalg.norm(w_new - w) < tol:
            convergiu = True
            return w_new, erros
        w = w_new
    return w, erros


#--------------------------------------------Método Gradiente Conjugado de Fletcher-Reeves------------------------------------------------------------------


# Método do Gradiente Conjugado de Fletcher-Reeves com histórico de erro
def gradiente_conjugado_com_erro(f0_init, fc_init, k_init, t, y, tol=1e-8, max_iter=10000):
    w = np.array([f0_init, fc_init, k_init], dtype=float)
    erros = []

    g = grad_J(w[0], w[1], w[2], t, y)
    d = -g  # direção inicial

    for i in range(max_iter):
        erro_atual = J(w[0], w[1], w[2], t, y)
        erros.append(erro_atual)

        if np.linalg.norm(g) < tol:
            break
            print("")

        # Passo fixo mais pequeno para evitar explodir
        alpha = 1e-7
        #alpha = linha_de_pesquisa_wolfe(w, d, t, y, g)

        w_new = w + alpha * d
        g_new = grad_J(w_new[0], w_new[1], w_new[2], t, y)

        beta = np.dot(g_new, g_new) / np.dot(g, g)
        d = -g_new + beta * d

        if np.linalg.norm(w_new - w) < tol:
            break
            print("fim")

        w = w_new
        g = g_new
    print(i)
    return w, erros

# #--------------------------------------------LER DADOS------------------------------------------------------------------

y = np.loadtxt("C:/Users/afons/Documents/Programacao/.vscode/Machine_Learning/dados.txt")  
t = 0.5 * np.arange(1, len(y) + 1)

#--------------------------------------------------------------------------------------------------------------------------


# Executar ambos os métodos
# Executar ambos os métodos com contagem de tempo
start = time.time()
params_grad, erros_grad = gradiente_com_erro(0, 0, 0.01, t, y)
tempo_grad = time.time() - start

start = time.time()
params_bfgs, erros_bfgs = bfgs_com_erro(0, 0, 0.01, t, y)
tempo_bfgs = time.time() - start

start = time.time()
#params_newton, erros_newton = newton_com_erro(0, 0, 0.01, t, y)
params_newton, erros_newton = newton_com_erro(0.08, 0.1, 0.01, t, y)
tempo_newton = time.time() - start

start = time.time()
#params_newtonarmijo, erros_newtonarmijo = newtonarmijo_com_erro(0, 0, 0.01, t, y)
params_newtonarmijo, erros_newtonarmijo = newtonarmijo_com_erro(0.08, 0.1, 0.01, t, y)
tempo_newtonarmijo = time.time() - start

start = time.time()
params_gradconj, erros_gradconj = gradiente_conjugado_com_erro(0, 0, 0.01, t, y)
tempo_gradconj = time.time() - start

# Gerar valores previstos pelo modelo
F_grad = F_horton(t, *params_grad)
F_bfgs = F_horton(t, *params_bfgs)
F_newton = F_horton(t, *params_newton)
F_newtonarmijo = F_horton(t, *params_newtonarmijo)
F_gradconj = F_horton(t, *params_gradconj)

#--------------------------------------------Gráfico Dados Reais vs Modelos------------------------------------------------------------------

# Plot dos dados reais e dos modelos ajustados
plt.figure(figsize=(7, 5))
plt.scatter(t, y, color='gray', label='Dados reais', s=15)
plt.plot(t, F_grad, color='blue', label='Modelo Gradiente', linewidth=2)
plt.plot(t, F_bfgs, color='red', label='Modelo BFGS', linewidth=2)
plt.plot(t, F_newton, color='green', label='Modelo Newton', linewidth=2)
plt.plot(t, F_newtonarmijo, color='yellow', label='Modelo Newton com condição de Armijo', linewidth=2)
plt.plot(t, F_gradconj, color='purple', label='Modelo Gradiente Conjugado de Fletcher-Reeves', linewidth=2)

plt.xlabel("Tempo (s)")
plt.ylabel("Infiltração acumulada (mm)")
plt.title("Ajuste da função de Horton aos dados")
plt.legend()
plt.grid(True, linestyle='--', alpha=0.5)
plt.tight_layout()
plt.show()

#--------------------------------------------Gráfico EQM vs Iterações------------------------------------------------------------------

# Plot comparativo dos erros (em escala log)
plt.plot(erros_grad, label='Gradiente', color='blue')
plt.plot(erros_bfgs, label='BFGS', color='red')
plt.plot(erros_newton, label='Newton', color='green')
plt.plot(erros_newtonarmijo, label='Newton com condição de Armijo', color='yellow')
plt.plot(erros_gradconj, label='Gradiente Conjugado', color='purple')
plt.xlabel("Número de iterações")
plt.ylabel("Erro Quadrático Médio (EQM)")
plt.title("Comparação da evolução do erro")
plt.yscale("log")
plt.grid(True, which="both", linestyle='--')
plt.legend()
plt.tight_layout()
plt.show()

#--------------------------------------------Apresentação resultados------------------------------------------------------------------
# Cálculo do EQM final para cada modelo com chamada compatível com J(f0, fc, k, t, y)
eqm_grad = J(params_grad[0], params_grad[1], params_grad[2], t, y)
eqm_bfgs = J(params_bfgs[0], params_bfgs[1], params_bfgs[2], t, y)
eqm_newton = J(params_newton[0], params_newton[1], params_newton[2], t, y)
eqm_newtonarmijo = J(params_newtonarmijo[0], params_newtonarmijo[1], params_newtonarmijo[2], t, y)
eqm_gradconj = J(params_gradconj[0], params_gradconj[1], params_gradconj[2], t, y)

# Impressão dos resultados
print("=== Resultados do Modelo de Horton ===\n")

print("Modelo do Gradiente:")
print(f"  f0 = {params_grad[0]:.6f}")
print(f"  fc = {params_grad[1]:.6f}")
print(f"  k  = {params_grad[2]:.6f}")
print(f"  EQM final = {eqm_grad:.6f} mm²")
print(f"  Tempo de execução = {tempo_grad:.6f} segundos")

print("\nModelo BFGS:")
print(f"  f0 = {params_bfgs[0]:.6f}")
print(f"  fc = {params_bfgs[1]:.6f}")
print(f"  k  = {params_bfgs[2]:.6f}")
print(f"  EQM final = {eqm_bfgs:.6f} mm²")
print(f"  Tempo de execução = {tempo_bfgs:.6f} segundos")

print("\nModelo Newton:")
print(f"  f0 = {params_newton[0]:.6f}")
print(f"  fc = {params_newton[1]:.6f}")
print(f"  k  = {params_newton[2]:.6f}")
print(f"  EQM final = {eqm_newton:.6f} mm²")
print(f"  Tempo de execução = {tempo_newton:.6f} segundos")

print("\nModelo Newton com condição de Armijo:")
print(f"  f0 = {params_newtonarmijo[0]:.6f}")
print(f"  fc = {params_newtonarmijo[1]:.6f}")
print(f"  k  = {params_newtonarmijo[2]:.6f}")
print(f"  EQM final = {eqm_newtonarmijo:.6f} mm²")
print(f"  Tempo de execução = {tempo_newtonarmijo:.6f} segundos")


print("\nModelo Gradiente Conjugado de Fletcher-Reeves:")
print(f"  f0 = {params_gradconj[0]:.6f}")
print(f"  fc = {params_gradconj[1]:.6f}")
print(f"  k  = {params_gradconj[2]:.6f}")
print(f"  EQM final = {eqm_gradconj:.6f} mm²")
print(f"  Tempo de execução = {tempo_gradconj:.6f} segundos")

f0_estimado = (y[1] - y[0]) / (t[1] - t[0])
fc_estimado = (y[-1] - y[-2]) / (t[-1] - t[-2])
#fc_estimado = (18.89690971 - 18.54126167) / (0.5) #ultimo valor tem ruido, é menos que o anterior e é impossivel taxa de infiltracao diminuir

print(f"\n  F0 Estimado = {f0_estimado:.6f} mm²")
print(f"  FC Estimado = {fc_estimado:.6f} mm²")