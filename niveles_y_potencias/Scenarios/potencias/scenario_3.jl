# Escenario 3
#-------------------------
# Normales

using Eta4Roc
using JSON
using Random 
using Distributions
using Base.Threads

function potencia_escenario_3()
    Random.seed!(1)
    B = 500
    MC = 1000
    alpha = 0.05
    n_values = [20, 50, 100]

    results = Dict{Any, Any}("title" => "Resultados de nivel del test para distintos tamaños muestrales. Escenario 3")

    for n in n_values
        p_val_auc = Vector{Float64}(undef, MC)
        p_val_youden = Vector{Float64}(undef, MC)
        p_val_no_param = Vector{Float64}(undef, MC)
        p_val_param = Vector{Float64}(undef, MC)
        p_val_kernel_opt = Vector{Float64}(undef, MC)
        p_val_kernel_hscv = Vector{Float64}(undef, MC)

        @threads for i in 1:MC
            controles = rand(Normal(0, 1), n)
            casos = rand(Normal(0.26, 0.3), n)

            auc_base = auc(controles, casos)
            youden_base = youden(controles, casos)
            no_param_base = non_parametric_eta(controles, casos)
            param_base = parametric_eta(controles, casos)
            kernel_opt_base = eta_kernel(controles, casos, "optimo")
            kernel_hscv_base = eta_kernel(controles, casos, "hscv")

            auc_bootstrap = Vector{Float64}(undef, B)
            youden_bootstrap = Vector{Float64}(undef, B)
            no_param_bootstrap = Vector{Float64}(undef, B)
            binormal_bootstrap = Vector{Float64}(undef, B)
            kernel_opt_bootstrap = Vector{Float64}(undef, B)
            kernel_hscv_bootstrap = Vector{Float64}(undef, B)

            for b in 1:B

                muestra = vcat(casos, controles)
                muestra = rand(muestra, 2 * n) 
                controles_b = muestra[1:n]
                casos_b = muestra[(n + 1):(2 * n)]
                auc_bootstrap[b] = auc(controles_b, casos_b)
                youden_bootstrap[b] = youden(controles_b, casos_b)
                no_param_bootstrap[b] = non_parametric_eta(controles_b, casos_b)
                binormal_bootstrap[b] = parametric_eta(controles_b, casos_b)
                kernel_opt_bootstrap[b] = eta_kernel(controles_b, casos_b, "optimo", mesh_size=300)
                kernel_hscv_bootstrap[b] = eta_kernel(controles_b, casos_b, "hscv", mesh_size=300)
            end

            p_val_auc[i] = mean(auc_bootstrap .>= auc_base)
            p_val_youden[i] = mean(youden_bootstrap .>= youden_base)
            p_val_no_param[i] = mean(no_param_bootstrap .>= no_param_base)
            p_val_param[i] = mean(binormal_bootstrap .>= param_base)
            p_val_kernel_opt[i] = mean(kernel_opt_bootstrap .>= kernel_opt_base)
            p_val_kernel_hscv[i] = mean(kernel_hscv_bootstrap .>= kernel_hscv_base)
        end

        nivel_auc = mean(p_val_auc .< alpha)
        nivel_youden = mean(p_val_youden .< alpha)
        nivel_no_param = mean(p_val_no_param .< alpha)
        nivel_param = mean(p_val_param .< alpha)
        nivel_kernel_opt = mean(p_val_kernel_opt .< alpha)
        nivel_kernel_hscv = mean(p_val_kernel_hscv .< alpha)

        results["N_$n"] = Dict(
            "auc" => nivel_auc,
            "youden" => nivel_youden,
            "no_param" => nivel_no_param,
            "param" => nivel_param,
            "kernel_opt" => nivel_kernel_opt,
            "kernel_hscv" => nivel_kernel_hscv
        )
    end

    output_dir = joinpath(@__DIR__, "..", "potencias", "jsons")
    if !isdir(output_dir)
        mkpath(output_dir)
    end

    output_path = joinpath(output_dir, "potencias_3.json")
    open(output_path, "w") do f
        JSON.print(f, results, 1)
    end
    println("================================================")
    println("============= ESCENARIO 3 FINALIZADO  ==========")
    println("================================================")
end
