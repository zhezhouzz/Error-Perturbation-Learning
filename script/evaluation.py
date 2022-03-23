import json
import subprocess
import sys
import argparse
import os
from datetime import datetime
from to_ind_figure import run_ind

verbose=True

config_file = "config/config.json"
benchmarks_config_file = "config/benchmarks.json"
qc_config = "config/qc_zero_knowledge_conf.json"
cmd_prefix = ["dune", "exec", "--", "main/main.exe"]
cmd_prefix2 = ["dune", "exec", "--profile", "release", "--", "main/main.exe"]

def solve_tap(p_setting):
    target_file = "/".join([p_setting['prefix'], p_setting['name'], p_setting['target']])
    assertion_file = "/".join([p_setting['prefix'], p_setting['name'], p_setting['assert']])
    pf_file = "/".join([p_setting['prefix'], p_setting['name'], p_setting['pf']])
    return target_file, assertion_file, pf_file

def invoc_cmd(cmd, output_file):
    if output_file is not None:
        # print("{}:{}".format(output_file, type(output_file)))
        if (verbose):
            print(" ".join(cmd + [">>", output_file]))
        with open(output_file, "a+") as ofile:
            try:
                subprocess.run(cmd, stdout=ofile)
            except subprocess.CalledProcessError as e:
                print(e.output)
    else:
        if (verbose):
            print(" ".join(cmd))
        try:
            subprocess.run(cmd)
        except subprocess.CalledProcessError as e:
            print(e.output)

def syn(p_setting, num, time, output_file):
    target_file, assertion_file, _ = solve_tap(p_setting)
    cmd = cmd_prefix + ["synthesize-time", config_file, target_file, assertion_file, num, time]
    invoc_cmd(cmd, output_file)

def eval_pf_time(p_setting, pf_file, time, outfile):
    target_file, assertion_file, pf_file_default = solve_tap(p_setting)
    if pf_file == None:
        pf_file = pf_file_default
    cmd = cmd_prefix + ["sampling-time", config_file, target_file, assertion_file, pf_file, time]
    invoc_cmd(cmd, outfile)

def eval_baseline_time(p_setting, qc_config, time, outfile):
    target_file, assertion_file, _ = solve_tap(p_setting)
    cmd = cmd_prefix + ["baseline-time", config_file, target_file, assertion_file, qc_config, time]
    invoc_cmd(cmd, outfile)

def eval_pf_num(p_setting, pf_file, num, outfile):
    target_file, assertion_file, pf_file_default = solve_tap(p_setting)
    print("fp_file: {}".format(pf_file))
    if pf_file == None:
        pf_file = pf_file_default
    cmd = cmd_prefix + ["eval-sampling", config_file, target_file, assertion_file, pf_file, num]
    invoc_cmd(cmd, outfile)

def eval_baseline_num(p_setting, qc_config, num, outfile):
    target_file, assertion_file, pf_file_default = solve_tap(p_setting)
    cmd = cmd_prefix + ["baseline", config_file, target_file, assertion_file, qc_config, num]
    invoc_cmd(cmd, outfile)

def eval_ind(p_setting, tname, s, e, num_bound):
    target_file, assertion_file, _ = solve_tap(p_setting)
    outfile = ".result/{}.ind".format(p_setting['name'])
    subprocess.run(["rm", outfile])
    outjsonfile = outfile + ".json"
    subprocess.run(["touch", outjsonfile])
    cmd = cmd_prefix2 + ["ind", config_file, target_file, assertion_file, outjsonfile, tname, s,e, num_bound]
    invoc_cmd(cmd, outfile)

def eval_robu_init(p_setting, qc_configfile, freq, num, num_bound):
    target_file, assertion_file, _ = solve_tap(p_setting)
    outfile = ".result/{}.robuinit".format(p_setting['name'])
    subprocess.run(["rm", outfile])
    outjsonfile = outfile + ".json"
    subprocess.run(["touch", outjsonfile])
    cmd = cmd_prefix2 + ["robu-init", config_file, target_file, assertion_file, qc_configfile, outjsonfile, freq, num, num_bound]
    invoc_cmd(cmd, outfile)

def eval_pie(name, qc_config, num_qc, num_qc2, bound):
    outfile = ".result/{}.pie".format(name)
    subprocess.run(["rm", outfile])
    cmd = cmd_prefix + ["pie", config_file, name, qc_config,  num_qc, num_qc2, bound]
    invoc_cmd(cmd, outfile)

def eval_moti(num_times, bound, outfile):
    target_file, assertion_file = "data/motivation/pf_enum.ml", "data/motivation/assertion1.ml"
    subprocess.run(["rm", outfile])
    cmd = cmd_prefix + ["moti-robu", config_file, target_file, assertion_file, num_times, bound]
    invoc_cmd(cmd, outfile)

if __name__ == "__main__":
    # print(os.path.isfile("zhouzhe"))
    with open(benchmarks_config_file) as f:
        bconfig = json.load(f)
    bconfig = bconfig["benchmarks"]
    parser = argparse.ArgumentParser()
    parser.add_argument("action", type=str,
                        help="the expected you want: syn | evalpf | evalqc")
    parser.add_argument("benchmarks", type=str,
                        help="the benchmarks you want to run: rbset | rbset,leftisthp | all")
    parser.add_argument("timebound", nargs='?', type=int, default=300,
                        help="the time bound in second, default is 300s")
    parser.add_argument("sizebound", nargs='?', type=int, default=30,
                        help="the size bound, default is 30")
    parser.add_argument("pffile", nargs='?', type=str, default=None,
                        help="the pf file")
    parser.add_argument("outfile", nargs='?', type=str, default=".out",
                        help="the output file")
    parser.add_argument("-v", "--verbose", action="store_true",
                        help="show executing commands")
    args = parser.parse_args()
    action = args.action
    if action == "moti":
        eval_moti("100", "1000", ".result/moti.robu")
        exit()
    if action == "pie":
        verbose=True
        subprocess.run(["mkdir", ".result"])
        for name in args.benchmarks.split(','):
            eval_pie(name, "config/pie_qc_conf.json", "100", "100", "100")
        exit()
    timebound = str(args.timebound)
    sizebound = str(args.sizebound)
    verbose=args.verbose
    outfile=args.outfile
    prefix = bconfig['prefix']
    for x in bconfig['meta']:
        x['prefix'] = bconfig['prefix']
    if args.benchmarks == "all":
        bs = bconfig['meta']
    else:
        names = args.benchmarks.split(',')
        bs = []
        for name in names:
            try:
                result = next(x for x in bconfig['meta'] if x['name'] == name)
                bs.append(result)
            except:
                print("cannot find the config of benchmark {}!".format(name))
                exit()
    if action == "syn":
        for b in bs:
            syn(b, "1", timebound, outfile)
    elif action == "evalpf":
        for b in bs:
            eval_pf_time(b, args.pffile, timebound, outfile)
    elif action == "syneval":
        for b in bs:
            subprocess.run(["rm", ".prog"])
            syn(b, "1", "300", ".prog")
            now = datetime.now()
            subprocess.run(["mv", ".logdir/.log", ".logdir/.log_" + b['name'] + "_" + now.strftime('%m%d%y-%T')])
            eval_pf_time(b, ".prog", "100", ".out")
    elif action == "evalpf":
        for b in bs:
            eval_pf_time(b, args.pffile, timebound, outfile)
    elif action == "evalbaseline":
        for b in bs:
            eval_baseline_time(b, qc_config, timebound, outfile)
    elif action == "evalpfnum":
        for b in bs:
            eval_pf_num(b, args.pffile, sizebound, outfile)
    elif action == "evalbaselinenum":
        for b in bs:
            eval_baseline_num(b, qc_config, sizebound, outfile)
    elif action == "ind":
        verbose=True
        subprocess.run(["mkdir", ".result"])
        for b in bs:
            eval_ind(b, "list", "3", "16", "500")
        names = [b['name']for b in bs]
    elif action == "robu-init":
        verbose=True
        subprocess.run(["mkdir", ".result"])
        for b in bs:
            eval_robu_init(b, "config/qc_conf.json", "1,2,4,8", "20", "500")
        names = [b['name']for b in bs]
    elif action == "ind_plot":
        verbose=True
        run_ind(names)
    # elif action == "moti":
    #     verbose=True
    #     subprocess.run(["mkdir", ".result"])
    #     for b in bs:
    #         eval_ind(b, "list", "3", "16", "500")
    #     names = [b['name']for b in bs]
    else:
        print("unknown command {}".format(action))
        exit()

# dune exec -- main/main.exe synthesize-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml 1 300 > .prog
# dune exec -- main/main.exe sampling-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml .prog 5
# dune exec -- main/main.exe baseline-time config/config.json data/client/rbset/balance.ml data/client/rbset/assertion1.ml config/qc_zero_knowledge_conf.json 5
