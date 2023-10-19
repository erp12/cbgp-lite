import boto3 
from datetime import datetime
from multiprocessing import Pool


BATCH_NAME = "baseline3"
EXTRA_ARGS = []
RUNS_PER_PROBLEM = 100
PARALLELISM = 100
PROBLEMS = [
    # "smallest",
    # "mirror-image",
    # "number-io",
    # "vectors-summed",
    # "negative-to-zero",
    # "median",
    # "vector-average",
    # "compare-string-lengths",
    # "last-index-of-zero",
    # "replace-space-with-newline",
    # "small-or-large",
    # "count-odds",
    # "digits",
    "for-loop-index",
]
CLUSTER = "cbgp-lite-runner"
TASK_ROLE_ARN = "arn:aws:iam::453734085325:role/ecsTaskExecutionRole"


ecs = boto3.client("ecs")
waiter = ecs.get_waiter("tasks_stopped")


def run_ecs_task(problem: str):
    response = ecs.run_task(
        cluster=CLUSTER,
        launchType="FARGATE",
        taskDefinition="cbgp-lite-task",
        overrides=dict(
            taskRoleArn=TASK_ROLE_ARN,
            executionRoleArn=TASK_ROLE_ARN,
            containerOverrides=[
                dict(
                    name="cbgp-lite",
                    command=["--problems", problem, "--opts", ":data-dir", "'\"/usr/data/psb\"'"] + EXTRA_ARGS
                )
            ],
        ),
        networkConfiguration=dict(
            awsvpcConfiguration=dict(
                subnets=["subnet-16d2b573", "subnet-3782d31d", "subnet-929dcfca", "subnet-1d1c9d11", "subnet-2be9e85d", "subnet-abbb8b96"],
                securityGroups=["sg-40cdba3b"],
                assignPublicIp="ENABLED",
            )
        ),
    )
    arn = response["tasks"][0]['taskArn']
    print(f"{problem=}; task {arn=}")
    waiter.wait(
        cluster=CLUSTER, 
        tasks=[arn],
        WaiterConfig=dict(
            Delay=30,
            MaxAttempts=600,
        )
    )


if __name__ == "__main__":
    print(f"START {datetime.now()}")
    # Get latest task definition.
    task_def = ecs.describe_task_definition(taskDefinition="cbgp-lite-task")["taskDefinition"]
    task_def_arn = task_def["taskDefinitionArn"]
    
    # If the batch name has changed, create new task definition to sepearate logs.
    target_log_group = f"/ecs/cbgp-lite/{BATCH_NAME}/"
    log_opts = task_def["containerDefinitions"][0]["logConfiguration"]["options"]
    if log_opts["awslogs-group"] != target_log_group:
        log_opts["awslogs-group"] = target_log_group
        for opt in ['compatibilities', 'registeredAt', 'registeredBy', 'status', 'revision', 'taskDefinitionArn', 'requiresAttributes']:
            task_def.pop(opt)
        response = ecs.register_task_definition(**task_def)
        task_def_arn = response["taskDefinition"]["taskDefinitionArn"]

    # Create run configs for the batch.
    runs = []
    for problem in PROBLEMS:
        for _ in range(RUNS_PER_PROBLEM):
            runs.append(problem)
    
    # Run all tasks
    with Pool(PARALLELISM) as p:
        for r in p.imap_unordered(run_ecs_task, runs):
            r
    print(f"END {datetime.now()}")
