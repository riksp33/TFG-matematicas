# Escenario 4
#-------------------------
# LogNormales

using Eta4Roc
using JSON
using Random 
using Distributions
using Base.Threads

function nivel_escenario_4()
    Random.seed!(1)
    n = 100
    B = 500
    MC = 1000
    alpha = 0.05


    p_val_auc = Vector{Float64}(undef, MC)
    p_val_youden = Vector{Float64}(undef, MC)
    p_val_no_param = Vector{Float64}(undef, MC)
    p_val_param = Vector{Float64}(undef, MC)
    p_val_kernel_opt = Vector{Float64}(undef, MC)
    p_val_kernel_hscv = Vector{Float64}(undef, MC)


    @threads for i in 1:MC
        # Estamos bajo H0
        controles = rand(LogNormal(0, 1), n)
        casos = rand(LogNormal(0, 1), n)

        auc_base = auc(controles, casos)
        youden_base = youden(controles, casos)
        no_param_base = non_parametric_eta(controles, casos)
        param_base = parametric_eta(controles, casos, bc = true)
        kernel_opt_base = eta_kernel(controles, casos, "optimo", bc = true)
        kernel_hscv_base = eta_kernel(controles, casos, "hscv", bc = true)

        auc_bootstrap = Vector{Float64}(undef, B)
        youden_bootstrap = Vector{Float64}(undef, B)
        no_param_bootstrap = Vector{Float64}(undef, B)
        binormal_bootstrap = Vector{Float64}(undef, B)
        kernel_opt_bootstrap = Vector{Float64}(undef, B)
        kernel_hscv_bootstrap = Vector{Float64}(undef, B)

        # Proceso de Bootstrap
        for b in 1:B
            controles_b = rand(controles, n)
            casos_b = rand(casos, n)
            auc_bootstrap[b] = auc(controles_b, casos_b)
            youden_bootstrap[b] = youden(controles_b, casos_b)
            no_param_bootstrap[b] = non_parametric_eta(controles_b, casos_b)
            binormal_bootstrap[b] = parametric_eta(controles_b, casos_b, bc = true)
            kernel_opt_bootstrap[b] = eta_kernel(controles_b, casos_b, "optimo", bc = true)
            kernel_hscv_bootstrap[b] = eta_kernel(controles_b, casos_b, "hscv", bc = true)
        end

        # Calculamos los valores p
        p_val_auc[i] = 1 - mean(auc_bootstrap .>= auc_base)
        p_val_youden[i] = 1 - mean(youden_bootstrap .>= youden_base)
        p_val_no_param[i] = 1 - mean(no_param_bootstrap .>= no_param_base)
        p_val_param[i] = 1 - mean(binormal_bootstrap .>= param_base)
        p_val_kernel_opt[i] = 1 - mean(kernel_opt_bootstrap .>= kernel_opt_base)
        p_val_kernel_hscv[i] = 1 - mean(kernel_hscv_bootstrap .>= kernel_hscv_base)
    end

    # Calculamos los niveles del test
    nivel_auc = mean(p_val_auc .< alpha)
    nivel_youden = mean(p_val_youden .< alpha)
    nivel_no_param = mean(p_val_no_param .< alpha)
    nivel_param = mean(p_val_param .< alpha)
    nivel_kernel_opt = mean(p_val_kernel_opt .< alpha)
    nivel_kernel_hscv = mean(p_val_kernel_hscv .< alpha)

    # Creamos el resultado como un diccionario
    results = Dict(
        "title" => "Resultados de nivel del test para tamaño muestral N = 50. Lognormales",
        "N_50" => Dict(
            "auc" => nivel_auc,
            "youden" => nivel_youden,
            "no_param" => nivel_no_param,
            "param" => nivel_param,
            "kernel_opt" => nivel_kernel_opt,
            "kernel_hscv" => nivel_kernel_hscv
        )
    )
    println(results)

    # Directorio de salida y archivo JSON
    output_dir = joinpath(@__DIR__, "..", "niveles", "jsons")
    if !isdir(output_dir)
        mkpath(output_dir)
    end

    output_path = joinpath(output_dir, "niveles_4.json")
    open(output_path, "w") do f
        JSON.print(f, results, 1)
    end
    

    # Mensaje final
    println("================================================")
    println("============= ESCENARIO 4 FINALIZADO  ==========")
    println("================================================")

end