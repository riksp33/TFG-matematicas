using Eta4Roc
using Distributions
using Base.Threads
using JSON
using Random

function scenario_9_potencia_two_biomarkers()
    Random.seed!(1)
    B = 500
    MC = 1000
    alpha = 0.05
    n_values = [20, 50, 100]
    correlation_enfermos_values = [0.0, 0.5]  # Valores de correlación a iterar
    results = Dict{Any, Any}("title" => "Resultados de potencia del test 2 marcadores para distintos tamaños muestrales. Escenario 9")

    for correlation_enfermos in correlation_enfermos_values
        # Crear un subdiccionario para esta correlación
        correlation_results = Dict{Any, Any}()

        for n in n_values

            p_val_auc = Vector{Float64}(undef, MC)
            p_val_youden = Vector{Float64}(undef, MC)
            p_val_param = Vector{Float64}(undef, MC)
            p_val_kernel_opt = Vector{Float64}(undef, MC)
            p_val_kernel_hscv = Vector{Float64}(undef, MC)

            # Distribution objects for markers 1 & 2
            m_1_sanos = 0.0
            m_2_sanos = 0.0

            m_1_enfermos = 0.74
            m_2_enfermos = 1.81

            v_1_sanos = 1.0
            v_2_sanos = 1.0

            v_1_enfermos = 1.0
            v_2_enfermos = 1.0

            correlation_sanos = 0.0 

            mean_vector_sanos = [m_1_sanos, m_2_sanos]
            mean_vector_enfermos = [m_1_enfermos, m_2_enfermos]

            cov_matrix_sanos = [v_1_sanos correlation_sanos; correlation_sanos v_2_sanos] 
            mv_normal_sanos = MvNormal(mean_vector_sanos, cov_matrix_sanos)

            cov_matrix_enfermos = [v_1_enfermos correlation_enfermos; correlation_enfermos v_2_enfermos] 
            mv_normal_enfermos = MvNormal(mean_vector_enfermos, cov_matrix_enfermos)

            @threads for i in 1:MC
                # Muestras de sanos
                muestra_mv_sanos = rand(mv_normal_sanos, n)
                sanos_marker_1 = muestra_mv_sanos[1,:]
                sanos_marker_2 = muestra_mv_sanos[2,:]

                # Muestras de enfermos
                muestra_mv_enfermos = rand(mv_normal_enfermos, n)
                enfermos_marker_1 = muestra_mv_enfermos[1,:]
                enfermos_marker_2 = muestra_mv_enfermos[2,:]

                auc_bootstrap_marker_1 = Vector{Float64}(undef, B)
                auc_bootstrap_marker_2 = Vector{Float64}(undef, B)
                youden_bootstrap_marker_1 = Vector{Float64}(undef, B)
                youden_bootstrap_marker_2 = Vector{Float64}(undef, B)
                binormal_bootstrap_marker_1 = Vector{Float64}(undef, B)
                binormal_bootstrap_marker_2 = Vector{Float64}(undef, B)
                kernel_opt_bootstrap_marker_1 = Vector{Float64}(undef, B)
                kernel_opt_bootstrap_marker_2 = Vector{Float64}(undef, B)
                kernel_hscv_bootstrap_marker_1 = Vector{Float64}(undef, B)
                kernel_hscv_bootstrap_marker_2 = Vector{Float64}(undef, B)

                for b in 1:B
                    indexes_sanos = rand(1:n, n )
                    indexes_enfermos = rand(1:n, n)

                    sanos_marker_1_b = sanos_marker_1[indexes_sanos]
                    sanos_marker_2_b = sanos_marker_2[indexes_sanos]

                    enfermos_marker_1_b = enfermos_marker_1[indexes_enfermos]
                    enfermos_marker_2_b = enfermos_marker_2[indexes_enfermos]

                    # AUC
                    auc_bootstrap_marker_1[b] = auc(sanos_marker_1_b, enfermos_marker_1_b)
                    auc_bootstrap_marker_2[b] = auc(sanos_marker_2_b, enfermos_marker_2_b)

                    # Youden
                    youden_bootstrap_marker_1[b] = youden(sanos_marker_1_b, enfermos_marker_1_b)
                    youden_bootstrap_marker_2[b] = youden(sanos_marker_2_b, enfermos_marker_2_b)

                    # Parametric
                    binormal_bootstrap_marker_1[b] = parametric_eta(sanos_marker_1_b, enfermos_marker_1_b)
                    binormal_bootstrap_marker_2[b] = parametric_eta(sanos_marker_2_b, enfermos_marker_2_b)

                    # Kernel opt
                    kernel_opt_bootstrap_marker_1[b] = eta_kernel(sanos_marker_1_b, enfermos_marker_1_b, "optimo")
                    kernel_opt_bootstrap_marker_2[b] = eta_kernel(sanos_marker_2_b, enfermos_marker_2_b, "optimo")

                    # Kernel hscv
                    kernel_hscv_bootstrap_marker_1[b] = eta_kernel(sanos_marker_1_b, enfermos_marker_1_b, "hscv")
                    kernel_hscv_bootstrap_marker_2[b] = eta_kernel(sanos_marker_2_b, enfermos_marker_2_b, "hscv")
                end

                delta_auc = auc_bootstrap_marker_2 .- auc_bootstrap_marker_1
                delta_youden = youden_bootstrap_marker_2 .- youden_bootstrap_marker_1
                delta_parametric = binormal_bootstrap_marker_2 .- binormal_bootstrap_marker_1
                delta_kernel_opt = kernel_opt_bootstrap_marker_2 .- kernel_opt_bootstrap_marker_1
                delta_kernel_hscv = kernel_hscv_bootstrap_marker_2 .- kernel_hscv_bootstrap_marker_1

                p_val_auc[i] = 2 * min(mean(delta_auc .< 0), mean(delta_auc .> 0))
                p_val_youden[i] = 2 * min(mean(delta_youden .< 0), mean(delta_youden .> 0))
                p_val_param[i] = 2 * min(mean(delta_parametric .< 0), mean(delta_parametric .> 0))
                p_val_kernel_opt[i] = 2 * min(mean(delta_kernel_opt .< 0), mean(delta_kernel_opt .> 0))
                p_val_kernel_hscv[i] = 2 * min(mean(delta_kernel_hscv .< 0), mean(delta_kernel_hscv .> 0))
            end

            nivel_auc = mean(p_val_auc .< alpha)
            nivel_youden = mean(p_val_youden .< alpha)
            nivel_param = mean(p_val_param .< alpha)
            nivel_kernel_opt = mean(p_val_kernel_opt .< alpha)
            nivel_kernel_hscv = mean(p_val_kernel_hscv .< alpha)

            correlation_results["N_$n"] = Dict(
                "auc" => nivel_auc,
                "youden" => nivel_youden,
                "param" => nivel_param,
                "kernel_opt" => nivel_kernel_opt,
                "kernel_hscv" => nivel_kernel_hscv)
        end

        # Añadir resultados de esta correlación al nodo principal
        results["correlation_$correlation_enfermos"] = correlation_results
    end

    output_dir = joinpath(@__DIR__, "jsons")
    if !isdir(output_dir)
        mkpath(output_dir)
    end

    output_path = joinpath(output_dir, "potencias_9.json")
    open(output_path, "w") do f
        JSON.print(f, results, 1)
    end
end
