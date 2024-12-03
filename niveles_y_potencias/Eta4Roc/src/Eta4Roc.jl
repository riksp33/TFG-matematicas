module Eta4Roc
using Distributions
using Statistics
using LinearAlgebra
using KernelDensitySJ
using Optim

export auc, youden, non_parametric_eta, parametric_eta, eta_kernel

function estandarizacion(x::Float64)::Float64
    return log(x + 1) / (1 + log(x + 1))
end

function obtener_mux(AUC::Float64, sigmax::Float64, sigmay::Float64, muy::Float64)::Float64
    return sqrt(sigmay^2 + sigmax^2) * quantile(Normal(), AUC) + muy
end

# AUC
function auc(controles::Vector{Float64}, casos::Vector{Float64})::Float64
    valores = vcat(controles, casos)
    etiquetas = vcat(zeros(Int, length(controles)), ones(Int, length(casos)))
    orden = sortperm(valores)
    etiquetas_ordenadas = etiquetas[orden]
    suma_rangos = sum(findall(x -> x == 1, etiquetas_ordenadas))
    n_controles = length(controles)
    n_casos = length(casos)
    auc_valor = (suma_rangos - (n_casos * (n_casos + 1)) / 2) / (n_casos * n_controles)

    return auc_valor
end

# Youden
function youden(controles::Vector{Float64}, casos::Vector{Float64})::Float64
    valores = vcat(controles, casos)
    puntos_corte = unique(sort(valores))
    mejor_youden = -Inf
    mejor_corte = nothing
    for corte in puntos_corte
        tp = sum(casos .>= corte)     
        fn = sum(casos .< corte)      
        tn = sum(controles .< corte)  
        fp = sum(controles .>= corte) 
        sensibilidad = tp / (tp + fn)
        especificidad = tn / (tn + fp)

        youden = sensibilidad + especificidad

        if youden > mejor_youden
            mejor_youden = youden
        end
    end

    return mejor_youden
end


function likbox(h, data, n)::Float64
    m = length(data) - n
    x = data[1:n]
    y = data[n+1:end]

    if abs(h) < 1e-5
        xh = log.(x)
        yh = log.(y)
    else
        xh = ((x .^ h) .- 1) ./ h
        yh = ((y .^ h) .- 1) ./ h
    end
    
    oout = -n/2 * log(sum((xh .- mean(xh)) .^ 2) / n) -
            m/2 * log(sum((yh .- mean(yh)) .^ 2) / m) +
            (h - 1) * (sum(log.(x)) + sum(log.(y)))
    return -oout
end

function hacer_box_cox(controles::Vector{Float64}, casos::Vector{Float64})
    if any(casos .<= 0) || any(controles .<= 0)
        constant = -min(minimum(casos), minimum(controles)) + 5e-4
        casos .+= constant
        controles .+= constant
    end

    h_ini = -0.6
    result = optimize(h -> likbox(h, vcat(casos, controles), length(casos)), -1.0, 1.0, Brent())
    hhat = Optim.minimizer(result)
    if abs(hhat) < 1e-5
        casos_t = log.(casos)
        controles_t = log.(controles)
    else
        casos_t = ((casos .^ hhat) .- 1) ./ hhat
        controles_t = ((controles .^ hhat) .- 1) ./ hhat
    end
    return controles_t, casos_t
end

function eta_pob_sd_stable(roc::Vector{Float64}, roc_prima::Vector{Float64}, mesh::StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}})::Float64
    etas = Vector{Float64}(undef, length(mesh))
    etas[1] = roc_prima[1] <= 1 ? 
        ((roc_prima[1] - 1)^2) * mesh[1] :
        ((roc_prima[1] - 1)^2) / roc_prima[1] * roc[1]

    for i in 2:length(mesh)
        etas[i] = roc_prima[i] <= 1 ? 
            ((roc_prima[i] - 1)^2) * (mesh[i] - mesh[i-1]) :
            ((roc_prima[i] - 1)^2) / roc_prima[i] * (roc[i] - roc[i-1])
    end
    eta = sum(etas)
    return estandarizacion(eta)
end


function non_parametric_eta(control::Vector{Float64}, casos::Vector{Float64})::Float64
    nd = length(casos)
    n_d_bar = length(control)
    control = sort(control)
    aux = zeros(Float64, n_d_bar + 1)
    aux[1] = sum(casos .<= control[1])
    aux[end] = sum(casos .> control[end])
    anterior = aux[1]
    for k in 2:n_d_bar
        actual = sum(casos .<= control[k])
        aux[k] = actual - anterior
        anterior = actual
    end
    eta = sum((aux ./ n_d_bar .- 1 / (n_d_bar + 1)).^2)
    return estandarizacion(eta)
end

function parametric_eta(controles::Vector{Float64}, casos::Vector{Float64}; bc::Bool = false, p=range(0.00001, stop=0.99999, length=10000))::Float64
    if bc
        controles, casos = hacer_box_cox(controles, casos)
    end
    mux, muy = mean(controles), mean(casos)
    sigmax, sigmay = std(controles), std(casos)
    ro = sigmax / sigmay
    delta = (muy - mux) / sigmay
    quantiles_normal = quantile.(Normal(), p)
    exp_neg_half_quantiles2 = exp.(-0.5 .* (quantiles_normal).^2)
    ROC = 1 .- cdf.(Normal(mux + delta * sigmax / ro, sigmax / ro), quantile.(Normal(mux, sigmax), 1 .- p))
    ROCprima = (ro .* exp.(-0.5 .* (delta .+ ro .* quantiles_normal).^2)) ./ exp_neg_half_quantiles2

    mesh_size = length(p)
    etaaux = [ROCprima[1] <= 1 ? (ROCprima[1] - 1)^2 * p[1] : (ROCprima[1] - 1)^2 / ROCprima[1] * ROC[1]]
    
    etaaux_rest = map(k -> begin
        if ROCprima[k] <= 1
            (ROCprima[k] - 1)^2 * (p[k] - p[k - 1])
        else
            (ROCprima[k] - 1)^2 / ROCprima[k] * (ROC[k] - ROC[k - 1])
        end
    end, 2:mesh_size)
    
    etaaux = vcat(etaaux, etaaux_rest)
    eta = sum(etaaux)
    return estandarizacion(eta)
end



function gaussian_kernel(x::Float64)
    return pdf(Normal(0, 1), x)
end

function DensidadKernel(datos::Vector{Float64}, puntos::StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, h::Float64) :: Vector{Float64}
    diffs = (puntos .- datos') / h
    matk = pdf.(Normal(0.0, 1.0), diffs)
    densidad = sum(matk, dims=2) / (length(datos) * h)
    return vec(densidad)
end

function DistKernel(datos::Vector{Float64}, puntos::StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}}, h::Float64) :: Vector{Float64}
    diffs = (puntos .- datos') / h
    matk = cdf.(Normal(0.0, 1.0), diffs)
    distribucion = sum(matk, dims=2) / length(datos)
    return vec(distribucion)
end
function Evaluate(punto::Float64, funcion::Vector{Float64}, mesh::StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}})::Float64
    index = searchsortedlast(mesh, punto)
    if index == length(mesh)
        return funcion[end]
    elseif index == 0
        return funcion[1]
    else
        return (funcion[index] + funcion[index + 1]) / 2
    end
end

function Inverse(punto::Float64, funcion::Vector{Float64}, mesh::StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}})::Float64
    index = searchsortedlast(funcion, punto)
    if index == length(funcion)
        return mesh[end]
    elseif index == 0
        return mesh[1]
    else
        return (mesh[index] + mesh[index + 1]) / 2
    end
end

function eta_kernel(muestra_sanos::Vector{Float64}, muestra_enfermos::Vector{Float64}, metodo::String; mesh_size::Int = 1000, bc::Bool = false)::Float64
    if bc
        muestra_sanos, muestra_enfermos = hacer_box_cox(muestra_sanos, muestra_enfermos)
    end

    m = vcat(muestra_sanos, muestra_enfermos)
    h = metodo == "optimo" ? 1.06 * std(m) * length(m)^(-1 / 5) : bwsj(m)

    # Generar mesh y calcular distribuciones
    mesh = (range(minimum(m), stop=maximum(m), length=mesh_size))
    estimated_dist_sanos = DistKernel(muestra_sanos, mesh, h)
    estimated_dist_enfermos = DistKernel(muestra_enfermos, mesh, h)
    estimated_dens_sanos = DensidadKernel(muestra_sanos, mesh, h)
    estimated_dens_enfermos = DensidadKernel(muestra_enfermos, mesh, h)

    # Generar p y p_opposite
    p = range(0.0001, stop=0.999, length=mesh_size)
    p_opp = 1 .- p

    # Calcular ROC y ROCprima
    inv = Inverse.(p_opp, Ref(estimated_dist_sanos), Ref(mesh))
    numeradores = Evaluate.(inv, Ref(estimated_dens_enfermos), Ref(mesh))
    denominadores = Evaluate.(inv, Ref(estimated_dens_sanos), Ref(mesh))
    roc = 1 .- Evaluate.(inv, Ref(estimated_dist_enfermos), Ref(mesh))
    roc_prima = numeradores ./ denominadores

    return eta_pob_sd_stable(roc, roc_prima, p)
end


end # module Eta4Roc
