import pymc3 as pm
import numpy as np

pop_delta_mean = 1 / 4
pop_delta_sd = 1 / 10

pop_wake_mean = 8
pop_wake_sd = 1 / 4

pop_sleep_mean = 20
pop_sleep_sd = 1 / 4

pop_mode_one_mean = 50
pop_mode_two_mean = 100
pop_mode_three_mean = 50

pop_mode_one_sd = 5
pop_mode_two_sd = 10
pop_mode_three_sd = 5

pop_y_one_sd = 5
pop_y_two_sd = 10
pop_y_three_sd = 5

birds = np.array([
    [
        [[1, 52], [2, 49], [3, 50]],
        [[1, 51], [2, 52], [3, 52]],
        [[1, 51], [2, 52], [3, 52], [4, 52]]
    ],
    [
        [[1, 52], [2, 49], [3, 50]],
        [[1, 51], [2, 52], [3, 52]],
        [[1, 51], [2, 52], [3, 52], [4, 52]],
        [[1, 51], [2, 52], [3, 52]],
    ],
]
)

total_birds = len(birds)
total_days = sum([len(bird) for bird in birds])
total_obs = 0

mode_one, mode_two, mode_three, wake, sleep, y = [total_days], [total_days], [total_days], [total_days], [total_days], \
                                                 [total_obs]

with pm.Model() as model:
    y_one_sd = pm.HalfNormal('y_one_sd', sd=pop_y_one_sd, shape=total_birds)
    y_two_sd = pm.HalfNormal('y_two_sd', sd=pop_y_two_sd, shape=total_birds)
    y_three_sd = pm.HalfNormal('y_three_sd', sd=pop_y_three_sd, shape=total_birds)

    delta = pm.Normal('delta_', mu=pop_delta_mean, sd=pop_delta_sd, shape=total_birds)
    wake_mean = pm.Normal('wake_mean', mu=pop_wake_mean, sd=pop_wake_sd, shape=total_birds)
    sleep_mean = pm.Normal('sleep_mean', mu=pop_sleep_mean, sd=pop_sleep_sd, shape=total_birds)
    wake_sd = pm.HalfNormal('wake_sd', sd=pop_wake_sd, shape=total_birds)
    sleep_sd = pm.HalfNormal('sleep_sd', sd=pop_sleep_sd, shape=total_birds)

    mode_one_mean = pm.Normal('mode_one_mean', mu=pop_mode_one_mean, sd=pop_mode_one_sd, shape=total_birds)
    mode_two_mean = pm.Normal('mode_two_mean', mu=pop_mode_two_mean, sd=pop_mode_two_sd, shape=total_birds)
    mode_three_mean = pm.Normal('mode_three_mean', mu=pop_mode_three_mean, sd=pop_mode_three_sd, shape=total_birds)

    mode_one_sd = pm.HalfNormal('mode_one_sd', sd=pop_mode_one_sd, shape=total_birds)
    mode_two_sd = pm.HalfNormal('mode_two_sd', sd=pop_mode_two_sd, shape=total_birds)
    mode_three_sd = pm.HalfNormal('mode_three_sd', sd=pop_mode_three_sd, shape=total_birds)

    for bird_idx, bird in enumerate(birds):
        for day_idx, day in enumerate(bird):
            bird_day_id = str(bird_idx) + "_" + str(day_idx)
            bird_day_idx = bird_idx + day_idx

            mode_one[bird_day_idx] = pm.Normal(bird_day_id + '_1', mu=mode_one_mean[bird_idx], sd=mode_one_sd[bird_idx])
            mode_two[bird_day_idx] = pm.Normal(bird_day_id + '_2', mu=mode_two_mean[bird_idx], sd=mode_two_sd[bird_idx])
            mode_three[bird_day_idx] = pm.Normal(bird_day_id + '_3', mu=mode_three_mean[bird_idx],
                                                 sd=mode_three_sd[bird_idx])

            wake[bird_day_idx] = pm.Normal(bird_day_id + '_W', mu=wake_mean[bird_idx], sd=wake_sd[bird_idx])
            sleep[bird_day_idx] = pm.Normal(bird_day_id + '_S', mu=sleep_mean[bird_idx], sd=sleep_sd[bird_idx])


            if (day_idx + 1) != len(bird):
                for obs_idx, obs in enumerate(day):
                    bird_day_obs_idx = bird_day_idx + obs_idx
                    bird_day_obs_id = bird_day_id + '_' + str(obs_idx)

                    #y[bird_day_obs_idx] = pm.Normal(bird_day_obs_id, mu=mu, sd=sd, observed=obs[1])
            else:
                for obs_idx in enumerate(day):
                    bird_day_obs_idx = bird_day_idx + obs_idx
                    bird_day_obs_id = bird_day_id + '_' + str(obs_idx)

                    #y[bird_day_obs_idx] = pm.Normal(bird_day_obs_id, mu=mu, sd=sd, observed=obs[1])

pm.model_to_graphviz(model)
